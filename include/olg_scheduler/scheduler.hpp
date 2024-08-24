/// Source: https://github.com/serges147/embedded_scheduler
///
/// Copyright (c) 2024 Zubax Robotics  <info@zubax.com>
///
/// Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated
/// documentation files (the "Software"), to deal in the Software without restriction, including without limitation
/// the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
/// and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
///
/// The above copyright notice and this permission notice shall be included in all copies or substantial portions of
/// the Software.
///
/// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE
/// WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS
/// OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
/// OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#pragma once

#include <cavl/cavl.hpp>

#include <cassert>
#include <chrono>
#include <concepts>
#include <optional>
#include <utility>

namespace olg_scheduler
{

template <typename TimePoint>
class Event;

/// Instances of this type are passed as the only argument to the user callbacks.
template <typename TimePoint>
struct Arg final
{
    /// Reference to the event that is being executed.
    Event<TimePoint>& event;
    /// Time when the event was scheduled to be executed (always in the past).
    TimePoint deadline;
    /// An approximation of the current time such that the following holds: deadline <= approx_now <= Clock::now().
    TimePoint approx_now;
};

/// User callbacks shall satisfy this concept.
template <typename Fun, typename TimePoint>
concept Callback = std::invocable<Fun, const Arg<TimePoint>&>;

/// Events are created on the stack.
/// The caller can destroy the event at any time, which will automatically cancel and remove it from the event loop.
/// If the event is single-shot or is otherwise canceled, the event object will keep lingering on the stack
/// until destroyed by the user (f.e. by `reset`-ing an optional which holds it).
///
/// The events are guaranteed to be executed strictly in the order of their deadlines;
/// equal deadlines are resolved strictly in the order of their registration (first registered -- first executed).
///
/// Internally, events are implemented as nodes on the AVL tree ordered by the deadline.
/// Whenever an event is fired, it is removed from the tree and re-inserted with the new deadline.
/// The event loop simply picks the leftmost node from the tree and checks if it is due.
/// The time complexity of all operations is logarithmic of the number of registered events.
///
/// B/c the event loop is not managing any memory for events, they may outlive the event loop -
/// they just won't be executed anymore as soon as the event loop has been destroyed.
/// Events could be created, cancelled or destroyed from callbacks (even for themselves).
template <typename TimePoint>
class Event : private cavl::Node<Event<TimePoint>>
{
    friend class cavl::Node<Event>;  // This friendship is required for the AVL tree implementation,
    friend class cavl::Tree<Event>;  // otherwise, we would have to use public inheritance, which is undesirable.

public:
    Event(const Event&)                      = delete;
    Event& operator=(const Event&)           = delete;
    Event& operator=(Event&& other) noexcept = delete;

    /// Cease future activities. No-op if not scheduled.
    /// It is safe to call this method more than once.
    /// It is safe to call this method from the callbacks.
    /// This method is automatically invoked by the destructor.
    void cancel() noexcept
    {
        /// It is guaranteed that while an event resides in the tree, it has a valid deadline set.
        if (deadline_)
        {
            // Removing a non-existent node from the tree is an UB that may lead to memory corruption,
            // so we have to check first if the event is actually registered.
            remove();
            // This is used to mark the event as unregistered so we don't double-remove it.
            // This can only be done after the event is removed from the tree.
            deadline_.reset();
        }
    }

    /// Diagnostic accessor for testing. Not intended for normal use.
    /// Empty option means that the event is not scheduled or canceled.
    /// It is guaranteed that while an event resides in the tree, it has a valid deadline set.
    [[nodiscard]] std::optional<TimePoint> getDeadline() const noexcept { return deadline_; }

protected:
    using Tree = cavl::Tree<Event>;
    using cavl::Node<Event>::remove;

    /// The event is not automatically scheduled, it must be explicitly scheduled by the user.
    Event() = default;

    /// The event can be safely destroyed at any time.
    /// It will be automatically canceled and removed from the event loop.
    virtual ~Event()
    {
        cancel();
        assert(!deadline_.has_value());
        assert((this->getChildNode(false) == nullptr) && (this->getChildNode(true) == nullptr) &&
               (this->getParentNode() == nullptr));
    }

    Event(Event&& other) noexcept :
        cavl::Node<Event>{std::move(static_cast<cavl::Node<Event>&>(other))},
        deadline_{std::exchange(other.deadline_, std::nullopt)}
    {}

    /// Ensure the event is in the tree and set the deadline to the specified absolute time point.
    /// If the event is already scheduled, the old deadline is overwritten.
    /// It is guaranteed that while an event resides in the tree, it has a valid deadline set.
    void schedule(const TimePoint dead, Tree& tree) noexcept
    {
        cancel();
        deadline_               = dead;  // The deadline shall be set before the event is inserted into the tree.
        const auto ptr_existing = tree.search(
            [dead](const Event& other) {
                /// No two deadlines compare equal, which allows us to have multiple nodes with the same deadline in
                /// the tree. With two nodes sharing the same deadline, the one added later is considered to be later.
                return (dead >= other.deadline_.value()) ? +1 : -1;
            },
            [this] { return this; });
        assert(std::get<0>(ptr_existing) == this);
        assert(!std::get<1>(ptr_existing));
        (void) ptr_existing;
    }

    /// The execution handler shall either reschedule or cancel the event
    /// (otherwise it will keep firing in a tight loop).
    /// If the event needs to be re-scheduled automatically, it must be done before the user callback is invoked,
    /// because the user may choose to cancel the event from the callback.
    virtual void execute(const Arg<TimePoint>& args, Tree& tree) = 0;

private:
    std::optional<TimePoint> deadline_;
};

/// This information is returned by the spin() method to allow the caller to decide what to do next
/// and assess the temporal performance of the event loop.
template <typename Clock>
struct SpinResult final
{
    /// The deadline of the next event to run (which is never in the past excepting a very short lag),
    /// or time_point::max() if there are no events.
    /// This can be used to let the application sleep when there are no events pending.
    typename Clock::time_point next_deadline;

    /// An approximation of the maximum lateness observed during the spin() call
    /// (the real slack may be worse than the approximation).
    /// This is always non-negative.
    typename Clock::duration worst_lateness;

    /// An approximation of the current time such that (approx_now <= Clock::now()).
    /// This is helpful because the time may be expensive to sample.
    typename Clock::time_point approx_now;
};

/// The event loop is used to execute activities at the specified time, either once or periodically.
/// The event handler callbacks are invoked with one argument of type Arg<Clock::time_point>.
/// Each factory method returns an event object by value,
/// which can be used to cancel the event by destroying it.
/// Event objects may outlive the event loop.
/// The time complexity of all operations is logarithmic of the number of registered events.
template <typename Clock>
class EventLoop final
{
public:
    using time_point = typename Clock::time_point;
    using duration   = typename Clock::duration;

private:
    /// This proxy is needed to expose the protected execute() method to the event loop.
    /// An alternative would be to make this class a friend of the Event class,
    /// or to make the execute() method public, which is undesirable.
    class EventProxy : public Event<time_point>
    {
    public:
        using Event<time_point>::execute;
    };

public:
    EventLoop()                 = default;
    EventLoop(const EventLoop&) = delete;
    EventLoop(EventLoop&&)      = delete;

    ~EventLoop() noexcept
    {
        // If this fails, it means that some events have outlived the event loop, which is not permitted.
        assert(isEmpty());
    }

    EventLoop& operator=(const EventLoop&) = delete;
    EventLoop& operator=(EventLoop&&)      = delete;

    /// The provided handler will be invoked with the specified interval starting from (now + period);
    /// if you also need to invoke it immediately, consider using defer().
    /// Returns the event handle on success, empty if out of memory or invalid inputs.
    template <Callback<time_point> Fun>
    [[nodiscard]] auto repeat(const duration period, Fun&& handler)
    {
        class Impl final : public EventProxy
        {
        public:
            Impl(EventLoop& owner, const duration per, Fun&& fun) : period_{per}, handler_{std::move(fun)}
            {
                this->schedule(Clock::now() + period_, owner.tree_);
            }

            void execute(const Arg<time_point>& args, Tree& tree) override
            {
                this->schedule(args.deadline + period_, tree);  // Strict period advancement, no phase error growth.
                handler_(args);
            }

        private:
            duration period_;
            Fun      handler_;
        };
        assert(period > duration::zero());
        return Impl{*this, period, std::forward<Fun>(handler)};
    }

    /// This is like repeat() with one crucial difference: the next deadline is defined not as (deadline+period),
    /// but as (approx_now+period), where (deadline <= approx_now <= now()).
    /// Therefore, the actual period is likely to be greater than requested and the phase error grows unboundedly.
    /// This mode is intended for tasks that do not require strict timing adherence but are potentially time-consuming,
    /// like interface polling or similar. Because the scheduler is allowed to let the phase slip, this type of event
    /// will automatically reduce the activation rate if the scheduler is CPU-starved, thus providing load regulation.
    template <Callback<time_point> Fun>
    [[nodiscard]] auto poll(const duration min_period, Fun&& handler)
    {
        class Impl final : public EventProxy
        {
        public:
            Impl(EventLoop& owner, const duration per, Fun&& fun) : min_period_{per}, handler_{std::move(fun)}
            {
                this->schedule(Clock::now() + min_period_, owner.tree_);
            }
            void execute(const Arg<time_point>& args, Tree& tree) override
            {
                this->schedule(args.approx_now + min_period_, tree);  // Accumulate phase error intentionally.
                handler_(args);
            }

        private:
            duration min_period_;
            Fun      handler_;
        };
        assert(min_period > duration::zero());
        return Impl{*this, min_period, std::forward<Fun>(handler)};
    }

    /// Like repeat() but the handler will be invoked only once and the event is canceled afterward.
    /// The deadline may be in the past, in which case the event will fire ASAP.
    template <Callback<time_point> Fun>
    [[nodiscard]] auto defer(const time_point deadline, Fun&& handler)
    {
        class Impl final : public EventProxy
        {
        public:
            Impl(EventLoop& owner, const time_point deadline, Fun&& fun) : handler_{std::move(fun)}
            {
                this->schedule(deadline, owner.tree_);
            }
            void execute(const Arg<time_point>& args, Tree&) override
            {
                this->cancel();
                handler_(args);
            }

        private:
            Fun handler_;
        };
        return Impl{*this, deadline, std::forward<Fun>(handler)};
    }

    /// Execute pending events strictly in the order of their deadlines until there are no pending events left.
    /// This method should be invoked regularly to pump the event loop.
    /// The method is optimized to make as few calls to Clock::now() as possible, as they may be expensive.
    ///
    /// The execution monitor is constructed when the event execution is commenced and destroyed when it is finished;
    /// it can be used to drive a test pad or some other timing fixture.
    template <typename ExecutionMonitor = std::monostate>
    [[nodiscard]] SpinResult<Clock> spin()
    {
        SpinResult<Clock> result{.next_deadline  = time_point::max(),
                                 .worst_lateness = duration::zero(),
                                 .approx_now     = time_point::min()};
        if (tree_.empty()) [[unlikely]]
        {
            result.approx_now = Clock::now();
            return result;
        }

        while (auto* const evt = static_cast<EventProxy*>(tree_.min()))
        {
            // The deadline is guaranteed to be set because it is in the tree.
            const auto deadline = evt->getDeadline().value();
            if (result.approx_now < deadline)  // Too early -- either we need to sleep or the time sample is obsolete.
            {
                result.approx_now = Clock::now();  // The number of calls to Clock::now() is minimized.
                if (result.approx_now < deadline)  // Nope, even with the latest time sample we are still early -- exit.
                {
                    result.next_deadline = deadline;
                    break;
                }
            }
            {
                ExecutionMonitor monitor{};  // RAII indication of the start and end of the event execution.
                // Execution will remove the event from the tree and then possibly re-insert it with a new deadline.
                evt->execute({.event = *evt, .deadline = deadline, .approx_now = result.approx_now}, tree_);
                (void) monitor;
            }
            result.next_deadline  = time_point::max();  // Reset the next deadline to the maximum possible value.
            result.worst_lateness = std::max(result.worst_lateness, result.approx_now - deadline);
        }

        assert(result.approx_now > time_point::min());
        assert(result.worst_lateness >= duration::zero());
        return result;
    }

    /// True if there are no registered events.
    [[nodiscard]] bool isEmpty() const noexcept { return tree_.empty(); }

    /// This intrusive accessor is only needed for testing and advanced diagnostics. Not intended for normal use.
    /// The nodes are ordered such that the earliest deadline is on the left.
    const auto& getTree() const noexcept { return tree_; }

private:
    using Tree = cavl::Tree<Event<time_point>>;

    Tree tree_;

};  // EventLoop

}  // namespace olg_scheduler
