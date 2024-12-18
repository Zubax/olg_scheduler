/// Source: https://github.com/zubax/olga_scheduler
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

#include "olga_scheduler.hpp"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <chrono>
#include <cstdint>
#include <optional>
#include <ratio>
#include <string>
#include <tuple>
#include <vector>

using testing::Gt;
using testing::Le;
using testing::Ne;
using testing::IsNull;
using testing::IsEmpty;
using testing::ElementsAre;
using testing::ElementsAreArray;

// NOLINTBEGIN(bugprone-unchecked-optional-access)
// NOLINTBEGIN(readability-function-cognitive-complexity, misc-const-correctness)
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers, readability-magic-numbers)

namespace
{

/// Expected type ID for the type returned from repeat(), poll() and defer().
constexpr std::array<std::uint8_t, 16> event_type_id =
    {0xB6, 0x87, 0x48, 0xA6, 0x7A, 0xDB, 0x4D, 0xF1, 0xB3, 0x1D, 0xA9, 0x8D, 0x50, 0xA7, 0x82, 0x47};

/// This clock has to keep global state to implement the TrivialClock trait.
class SteadyClockMock final
{
public:
    using rep        = std::int64_t;
    using period     = std::ratio<1, 1'000>;
    using duration   = std::chrono::duration<rep, period>;
    using time_point = std::chrono::time_point<SteadyClockMock>;

    [[maybe_unused]] static constexpr bool is_steady = true;

    static time_point& now() noexcept
    {
        static time_point g_now_;
        return g_now_;
    }

    static void reset() noexcept { now() = {}; }

    template <typename Rep, typename Per>
    static void advance(const std::chrono::duration<Rep, Per> dur)
    {
        now() += std::chrono::duration_cast<duration>(dur);
    }
};

}  // namespace

namespace olga_scheduler::verification
{

TEST(TestOlgaScheduler, EventLoopBasic)
{
    using std::chrono_literals::operator""ms;

    // Initial configuration of the clock.
    SteadyClockMock::reset();
    SteadyClockMock::advance(10'000ms);

    using Loop = EventLoop<SteadyClockMock>;
    using Arg  = Arg<Loop::time_point>;
    Loop evl;

    std::optional<Arg> a;
    std::optional<Arg> b;
    std::optional<Arg> c;
    std::optional<Arg> d;

    auto out = evl.spin();  // Nothing to do.
    EXPECT_THAT(out.next_deadline, SteadyClockMock::time_point::max());
    EXPECT_THAT(out.worst_lateness, SteadyClockMock::duration::zero());
    EXPECT_THAT(out.approx_now.time_since_epoch(), 10'000ms);
    EXPECT_TRUE(evl.isEmpty());
    EXPECT_THAT(evl.getTree()[0U], IsNull());

    // Register our handlers. Events with same deadline are ordered such that the one added later is processed later.
    auto evt_a = evl.repeat(1000ms, [&](const auto& arg) { a.emplace(arg); });
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 11'000ms);
    EXPECT_THAT(evl.getTree()[1U], IsNull());
    EXPECT_FALSE(evl.isEmpty());

    // Check the type ID of return type repeat().
    EXPECT_THAT(decltype(evt_a)::_get_type_id_(), ElementsAreArray(event_type_id));

    auto evt_b = evl.repeat(100ms,  // Smaller deadline goes on the left.
                            [&](const auto& arg) { b.emplace(arg); });
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 10'100ms);
    EXPECT_THAT(evl.getTree()[1U]->getDeadline().time_since_epoch(), 11'000ms);
    EXPECT_THAT(evl.getTree()[2U], IsNull());

    auto evt_c = evl.defer(SteadyClockMock::now() + 2000ms, [&](const auto& arg) { c.emplace(arg); });
    // EXPECT_THAT(evt_c, Optional(_));
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 10'100ms);
    EXPECT_THAT(evl.getTree()[1U]->getDeadline().time_since_epoch(), 11'000ms);
    const auto* const f3 = evl.getTree()[2U];
    EXPECT_THAT(f3->getDeadline().time_since_epoch(), 12'000ms);  // New entry.
    EXPECT_THAT(evl.getTree()[3U], IsNull());

    auto evt_d = evl.defer(SteadyClockMock::now() + 2000ms,  // Same deadline!
                           [&](const auto& arg) { d.emplace(arg); });
    // EXPECT_THAT(evt_d, NotNull());
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 10'100ms);
    EXPECT_THAT(evl.getTree()[1U]->getDeadline().time_since_epoch(), 11'000ms);
    EXPECT_THAT(evl.getTree()[2U], f3);  // Entry added before this one.
    const auto* const f4 = evl.getTree()[3U];
    EXPECT_THAT(f3, Ne(f4));
    EXPECT_THAT(f4->getDeadline().time_since_epoch(), 12'000ms);  // New entry, same deadline added later.
    EXPECT_THAT(evl.getTree()[4U], IsNull());

    // Poll but there are no pending Events yet.
    out = evl.spin();
    EXPECT_THAT(out.next_deadline.time_since_epoch(), 10'100ms);
    EXPECT_THAT(out.worst_lateness, 0ms);
    EXPECT_THAT(out.approx_now.time_since_epoch(), 10'000ms);
    EXPECT_THAT(evl.getTree().size(), 4);
    EXPECT_FALSE(a);
    EXPECT_FALSE(b);
    EXPECT_FALSE(c);
    EXPECT_FALSE(d);

    // Make the first two expire. The one-shot two are still pending.
    SteadyClockMock::advance(1100ms);
    EXPECT_THAT(SteadyClockMock::now().time_since_epoch(), 11'100ms);
    out = evl.spin();
    EXPECT_THAT(out.next_deadline.time_since_epoch(), 11'200ms);
    EXPECT_THAT(out.worst_lateness, 1000ms);
    EXPECT_THAT(out.approx_now.time_since_epoch(), 11'100ms);
    EXPECT_THAT(evl.getTree().size(), 4);
    EXPECT_TRUE(a);
    EXPECT_THAT(a.value().deadline.time_since_epoch(), 11'000ms);
    EXPECT_TRUE(b);
    EXPECT_THAT(b.value().deadline.time_since_epoch(), 11'100ms);
    EXPECT_FALSE(c);
    EXPECT_FALSE(d);
    a.reset();
    b.reset();
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 11'200ms);

    // Move on. Let C&D fire, they are canceled automatically.
    SteadyClockMock::advance(900ms);
    EXPECT_THAT(SteadyClockMock::now().time_since_epoch(), 12'000ms);
    out = evl.spin();
    EXPECT_THAT(out.next_deadline.time_since_epoch(), 12'100ms);
    EXPECT_THAT(out.worst_lateness, 800ms);
    EXPECT_THAT(out.approx_now.time_since_epoch(), 12'000ms);
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 12'100ms);
    EXPECT_THAT(evl.getTree().size(), 2);  // C&D have left us.
    EXPECT_TRUE(a);
    EXPECT_THAT(a.value().deadline.time_since_epoch(), 12'000ms);
    EXPECT_TRUE(b);
    EXPECT_THAT(b.value().deadline.time_since_epoch(), 12'000ms);
    EXPECT_TRUE(c);
    EXPECT_THAT(c.value().deadline.time_since_epoch(), 12'000ms);
    EXPECT_TRUE(d);
    EXPECT_THAT(d.value().deadline.time_since_epoch(), 12'000ms);
    a.reset();
    b.reset();
    c.reset();
    d.reset();
    // Ensure the deadline is cleared on those events that are canceled.
    EXPECT_THAT(evt_a.getDeadline(), Gt(Loop::time_point::min()));
    EXPECT_THAT(evt_b.getDeadline(), Gt(Loop::time_point::min()));
    EXPECT_THAT(evt_c.getDeadline(), Loop::time_point::min());
    EXPECT_THAT(evt_d.getDeadline(), Loop::time_point::min());

    // Drop the second event and ensure it is removed from the tree immediately.
    SteadyClockMock::advance(1050ms);
    EXPECT_THAT(SteadyClockMock::now().time_since_epoch(), 13'050ms);
    EXPECT_THAT(evt_b.getDeadline(), Gt(Loop::time_point::min()));
    evt_b.cancel();
    EXPECT_THAT(evt_b.getDeadline(), Loop::time_point::min());  // Unregistered, cleared.
    EXPECT_THAT(evl.getTree().size(), 1);                       // Freed already.
    evt_b.cancel();                                             // Idempotency.
    EXPECT_THAT(evt_b.getDeadline(), Loop::time_point::min());  // Ditto.
    EXPECT_THAT(evl.getTree().size(), 1);                       // Ditto.
    out = evl.spin();
    EXPECT_THAT(out.next_deadline.time_since_epoch(), 14'000ms);  // B removed so the next one is A.
    EXPECT_THAT(out.worst_lateness, 50ms);
    EXPECT_THAT(out.approx_now.time_since_epoch(), 13'050ms);
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 14'000ms);
    EXPECT_THAT(1, evl.getTree().size());  // Second dropped.
    EXPECT_TRUE(a);
    EXPECT_THAT(a.value().deadline.time_since_epoch(), 13'000ms);
    EXPECT_FALSE(b);
    EXPECT_FALSE(c);
    EXPECT_FALSE(d);
    a.reset();

    // Nothing to do yet.
    out = evl.spin();
    EXPECT_THAT(out.next_deadline.time_since_epoch(), 14'000ms);  // Same up.
    EXPECT_THAT(out.worst_lateness, 0ms);
    EXPECT_THAT(out.approx_now.time_since_epoch(), 13'050ms);
    EXPECT_FALSE(a);
    EXPECT_FALSE(b);
    EXPECT_FALSE(c);
    EXPECT_FALSE(d);
}

TEST(TestOlgaScheduler, EventLoopTotalOrdering)
{
    using std::chrono_literals::operator""ms;

    SteadyClockMock::reset();
    EventLoop<SteadyClockMock> evl;
    std::uint8_t               a     = 0;
    std::uint8_t               b     = 0;
    std::uint8_t               c     = 0;
    const auto                 evt_a = evl.repeat(10ms, [&](const auto&) {
        a++;
        EXPECT_THAT(a, Gt(b));
        EXPECT_THAT(a, Gt(c));
    });
    const auto                 evt_b = evl.repeat(10ms, [&](const auto&) {
        b++;
        EXPECT_THAT(b, Le(a));
        EXPECT_THAT(b, Gt(c));
    });
    const auto                 evt_c = evl.repeat(10ms, [&](const auto&) {
        c++;
        EXPECT_THAT(c, Le(a));
        EXPECT_THAT(c, Le(b));
    });
    SteadyClockMock::advance(50ms);
    (void) evl.spin();
    EXPECT_THAT(a, 5);
    EXPECT_THAT(b, 5);
    EXPECT_THAT(c, 5);
}

TEST(TestOlgaScheduler, EventLoopPoll)
{
    using time_point = SteadyClockMock::time_point;
    using std::chrono_literals::operator""ms;
    SteadyClockMock::reset();
    EventLoop<SteadyClockMock> evl;

    std::optional<time_point> last_tp{};
    SteadyClockMock::advance(100ms);
    auto evt = evl.poll(10ms, [&](const auto& arg) {
        EXPECT_FALSE(last_tp);
        last_tp = arg.deadline;
    });
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 110ms);

    // Check the type ID of return type poll().
    EXPECT_THAT(decltype(evt)::_get_type_id_(), ElementsAreArray(event_type_id));

    SteadyClockMock::advance(30ms);
    EXPECT_THAT(SteadyClockMock::now().time_since_epoch(), 130ms);
    (void) evl.spin();
    EXPECT_TRUE(last_tp);
    EXPECT_THAT(last_tp.value().time_since_epoch(), 110ms);
    last_tp.reset();
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 140ms);  // Skipped ahead!

    SteadyClockMock::advance(70ms);
    EXPECT_THAT(SteadyClockMock::now().time_since_epoch(), 200ms);
    (void) evl.spin();
    EXPECT_TRUE(last_tp);
    EXPECT_THAT(last_tp.value().time_since_epoch(), 140ms);
    last_tp.reset();
    EXPECT_THAT(evl.getTree()[0U]->getDeadline().time_since_epoch(), 210ms);  // Skipped ahead!
}

TEST(TestOlgaScheduler, EventLoopDefer_single_overdue)
{
    using time_point = SteadyClockMock::time_point;
    using std::chrono_literals::operator""ms;
    SteadyClockMock::reset();
    EventLoop<SteadyClockMock> evl;

    auto evt = evl.defer(SteadyClockMock::now() + 1000ms, [](const auto&) {});

    // Check the type ID of return type defer().
    EXPECT_THAT(decltype(evt)::_get_type_id_(), ElementsAreArray(event_type_id));

    // This is special case - only one deferred event (and no "repeat"-s!), and it is already overdue (by +30ms).
    // So, `next_deadline` should be `time_point::max()` b/c there will be nothing left pending after spin.
    SteadyClockMock::advance(1000ms + 30ms);
    const auto out = evl.spin();
    EXPECT_THAT(out.next_deadline.time_since_epoch(), time_point::max().time_since_epoch());
    EXPECT_THAT(out.worst_lateness, 30ms);
    EXPECT_THAT(out.approx_now.time_since_epoch(), 1030ms);
}

TEST(TestOlgaScheduler, EventLoopDefer_long_running_callback)
{
    using duration = SteadyClockMock::duration;
    using std::chrono_literals::operator""ms;
    SteadyClockMock::reset();
    EventLoop<SteadyClockMock> evl;

    std::vector<std::tuple<std::string, duration, duration>> calls;

    auto evt_a = evl.defer(SteadyClockMock::now() + 0ms, [&](const auto& arg) {  //
        // Emulate that it took whole 100ms to execute "a" callback,
        // so it will be already overdue for the next "b" event - should be executed as well.
        calls.emplace_back("a", arg.deadline.time_since_epoch(), arg.approx_now.time_since_epoch());
        SteadyClockMock::advance(100ms);
    });
    auto evt_b = evl.defer(SteadyClockMock::now() + 20ms, [&](const auto& arg) {  //
        calls.emplace_back("b", arg.deadline.time_since_epoch(), arg.approx_now.time_since_epoch());
    });

    const auto out = evl.spin();
    EXPECT_THAT(out.next_deadline.time_since_epoch(), duration::max());
    EXPECT_THAT(out.worst_lateness, 80ms);
    EXPECT_THAT(out.approx_now.time_since_epoch(), 100ms);

    EXPECT_THAT(calls,
                ElementsAre(std::make_tuple("a", 0ms, 0ms),  //
                            std::make_tuple("b", 20ms, 100ms)));
}

TEST(TestOlgaScheduler, HandleMovement)
{
    using duration = SteadyClockMock::duration;
    using std::chrono_literals::operator""ms;

    SteadyClockMock::reset();

    EventLoop<SteadyClockMock> evl;

    std::vector<std::tuple<std::string, duration, duration>> calls;

    std::optional a = evl.repeat(330ms, [&](const auto& arg) {
        calls.emplace_back("a", arg.deadline.time_since_epoch(), arg.approx_now.time_since_epoch());
    });
    std::optional b = evl.repeat(333ms, [&](const auto& arg) {
        calls.emplace_back("b", arg.deadline.time_since_epoch(), arg.approx_now.time_since_epoch());
    });
    std::optional c = evl.repeat(337ms, [&](const auto& arg) {
        calls.emplace_back("c", arg.deadline.time_since_epoch(), arg.approx_now.time_since_epoch());
    });
    EXPECT_THAT(evl.getTree().size(), 3);

    SteadyClockMock::advance(1000ms);
    EXPECT_TRUE(a);
    a.reset();  // Destroy a
    EXPECT_FALSE(a);
    (void) evl.spin();
    EXPECT_THAT(evl.getTree().size(), 2);
    EXPECT_THAT(calls,
                ElementsAre(std::make_tuple("b", 333ms * 1, 1000ms),  //
                            std::make_tuple("c", 337ms * 1, 1000ms),
                            std::make_tuple("b", 333ms * 2, 1000ms),
                            std::make_tuple("c", 337ms * 2, 1000ms),
                            std::make_tuple("b", 333ms * 3, 1000ms)));
    calls.clear();

    EXPECT_TRUE(b);
    auto d = std::move(b);  // b moved into d
    EXPECT_TRUE(d);

    SteadyClockMock::advance(1000ms);
    (void) evl.spin();
    EXPECT_THAT(evl.getTree().size(), 2);  // No change -- references moved but inferiors are kept alive.
    EXPECT_THAT(calls,
                ElementsAre(std::make_tuple("c", 337ms * 3, 2000ms),
                            std::make_tuple("b", 333ms * 4, 2000ms),
                            std::make_tuple("c", 337ms * 4, 2000ms),
                            std::make_tuple("b", 333ms * 5, 2000ms),
                            std::make_tuple("c", 337ms * 5, 2000ms),
                            std::make_tuple("b", 333ms * 6, 2000ms)));
    calls.clear();

    EXPECT_TRUE(c);
    c.reset();  // Destroy c
    EXPECT_FALSE(c);

    SteadyClockMock::advance(1000ms);
    (void) evl.spin();
    EXPECT_THAT(evl.getTree().size(), 1);  // c destroyed, only b left alive (now in d).
    EXPECT_THAT(calls,
                ElementsAre(std::make_tuple("b", 333ms * 7, 3000ms),
                            std::make_tuple("b", 333ms * 8, 3000ms),
                            std::make_tuple("b", 333ms * 9, 3000ms)));
    calls.clear();

    d.reset();
    SteadyClockMock::advance(1000ms);
    (void) evl.spin();
    EXPECT_THAT(evl.getTree().size(), 0);
    EXPECT_THAT(calls, IsEmpty());
}

}  // namespace olga_scheduler::verification

// NOLINTEND(cppcoreguidelines-avoid-magic-numbers, readability-magic-numbers)
// NOLINTEND(readability-function-cognitive-complexity, misc-const-correctness)
// NOLINTEND(bugprone-unchecked-optional-access)
