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

#include "embedded_scheduler/scheduler.hpp"

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <chrono>
#include <cstdint>
#include <optional>
#include <ratio>

using testing::IsNull;
using testing::NotNull;

// NOLINTBEGIN(readability-function-cognitive-complexity, misc-const-correctness)
// NOLINTBEGIN(cppcoreguidelines-avoid-magic-numbers, readability-magic-numbers)

namespace
{

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

namespace embedded_scheduler::verification
{
TEST(TestEmbeddedScheduler, EventLoopBasic)
{
    using std::chrono_literals::operator""ms;

    // Initial configuration of the clock.
    SteadyClockMock::reset();
    SteadyClockMock::advance(10'000ms);

    using Loop = EventLoop<SteadyClockMock>;
    using Arg  = Arg<Loop::time_point>;
    std::optional<Loop> evl;
    evl.emplace();

    std::optional<Arg> a;
    std::optional<Arg> b;
    std::optional<Arg> c;
    std::optional<Arg> d;

    auto out = evl->spin();  // Nothing to do.
    // semihost::log(__LINE__, " ", out);
    EXPECT_THAT(out.next_deadline, SteadyClockMock::time_point::max());
    EXPECT_THAT(out.worst_lateness, SteadyClockMock::duration::zero());
    EXPECT_TRUE(evl->isEmpty());
    EXPECT_THAT(evl->getTree()[0U], IsNull());

    // Ensure invalid arguments are rejected.
    EXPECT_THAT(evl->repeat(0ms, [&](auto tp) { a.emplace(tp); }), IsNull());  // Period shall be positive.
    EXPECT_TRUE(evl->isEmpty());

    // Register our handlers. Events with same deadline are ordered such that the one added later is processed later.
    // semihost::log("Alloc ", __LINE__, ": ", platform::heap::getDiagnostics().allocated);
    auto evt_a = evl->repeat(1000ms, [&](auto tp) {
        a.emplace(tp);
        // semihost::log("A! ", tp);
    });
    EXPECT_THAT(evt_a, NotNull());
    EXPECT_THAT(evl->getTree()[0U]->getDeadline().value().time_since_epoch(), 11'000ms);
    EXPECT_THAT(evl->getTree()[1U], IsNull());
    EXPECT_FALSE(evl->isEmpty());

    // semihost::log("Alloc ", __LINE__, ": ", platform::heap::getDiagnostics().allocated);
    auto evt_b = evl->repeat(100ms,  // Smaller deadline goes on the left.
                             [&](auto tp) {
                                 b.emplace(tp);
                                 // semihost::log("B! ", tp);
                             });
    EXPECT_THAT(evt_b, NotNull());
    EXPECT_THAT(evl->getTree()[0U]->getDeadline().value().time_since_epoch(), 10'100ms);
    EXPECT_THAT(evl->getTree()[1U]->getDeadline().value().time_since_epoch(), 11'000ms);
    EXPECT_THAT(evl->getTree()[2U], IsNull());
/*
    // semihost::log("Alloc ", __LINE__, ": ", platform::heap::getDiagnostics().allocated);
    auto evt_c = evl->defer(SteadyClockMock::now() + 2000ms, [&](auto tp) {
        c.emplace(tp);
        // semihost::log("C! ", tp);
    });
    TEST_ASSERT_NOT_NULL(evt_c);
    TEST_ASSERT_EQUAL(10'100ms, evl->getTree()[0U]->getDeadline().value().time_since_epoch());
    TEST_ASSERT_EQUAL(11'000ms, evl->getTree()[1U]->getDeadline().value().time_since_epoch());
    const auto* const f3 = evl->getTree()[2U];
    TEST_ASSERT_EQUAL(12'000ms, f3->getDeadline().value().time_since_epoch());  // New entry.
    TEST_ASSERT_NULL(evl->getTree()[3U]);

    // semihost::log("Alloc ", __LINE__, ": ", platform::heap::getDiagnostics().allocated);
    auto evt_d = evl->defer(SteadyClockMock::now() + 2000ms,  // Same deadline!
                            [&](auto tp) {
                                d.emplace(tp);
                                // semihost::log("D! ", tp);
                            });
    TEST_ASSERT_NOT_NULL(evt_d);
    TEST_ASSERT_EQUAL(10'100ms, evl->getTree()[0U]->getDeadline().value().time_since_epoch());
    TEST_ASSERT_EQUAL(11'000ms, evl->getTree()[1U]->getDeadline().value().time_since_epoch());
    TEST_ASSERT_EQUAL(f3, evl->getTree()[2U]);  // Entry added before this one.
    const auto* const f4 = evl->getTree()[3U];
    TEST_ASSERT(f3 != f4);
    TEST_ASSERT_EQUAL(12'000ms, f4->getDeadline().value().time_since_epoch());  // New entry, same deadline added later.
    TEST_ASSERT_NULL(evl->getTree()[4U]);

    // Poll but there are no pending Events yet.
    out = evl->spin();
    // semihost::log(__LINE__, " ", out);
    TEST_ASSERT_EQUAL(10'100ms, out.next_deadline.time_since_epoch());
    TEST_ASSERT_EQUAL(0ms, out.worst_lateness);
    TEST_ASSERT_EQUAL_UINT64(4, evl->getTree().size());
    TEST_ASSERT_FALSE(a);
    TEST_ASSERT_FALSE(b);
    TEST_ASSERT_FALSE(c);
    TEST_ASSERT_FALSE(d);

    // Make the first two expire. The one-shot two are still pending.
    SteadyClockMock::advance(1100ms);
    TEST_ASSERT_EQUAL(11'100ms, SteadyClockMock::now().time_since_epoch());
    out = evl->spin();
    // semihost::log(__LINE__, " ", out);
    TEST_ASSERT_EQUAL(11'200ms, out.next_deadline.time_since_epoch());
    TEST_ASSERT_EQUAL(1000ms, out.worst_lateness);
    TEST_ASSERT_EQUAL_UINT64(4, evl->getTree().size());
    TEST_ASSERT_TRUE(a);
    TEST_ASSERT_EQUAL(11'000ms, a.value().deadline.time_since_epoch());
    TEST_ASSERT_TRUE(b);
    TEST_ASSERT_EQUAL(11'100ms, b.value().deadline.time_since_epoch());
    TEST_ASSERT_FALSE(c);
    TEST_ASSERT_FALSE(d);
    a.reset();
    b.reset();
    TEST_ASSERT_EQUAL(11'200ms, evl->getTree()[0U]->getDeadline().value().time_since_epoch());

    // Move on. Let C&D fire, they are canceled automatically.
    SteadyClockMock::advance(900ms);
    TEST_ASSERT_EQUAL(12'000ms, SteadyClockMock::now().time_since_epoch());
    out = evl->spin();
    // semihost::log(__LINE__, " ", out);
    TEST_ASSERT_EQUAL(12'100ms, out.next_deadline.time_since_epoch());
    TEST_ASSERT_EQUAL(800ms, out.worst_lateness);
    TEST_ASSERT_EQUAL(12'100ms, evl->getTree()[0U]->getDeadline().value().time_since_epoch());
    TEST_ASSERT_EQUAL_UINT64(2, evl->getTree().size());  // C&D have left us.
    TEST_ASSERT_TRUE(a);
    TEST_ASSERT_EQUAL(12'000ms, a.value().deadline.time_since_epoch());
    TEST_ASSERT_TRUE(b);
    TEST_ASSERT_EQUAL(12'000ms, b.value().deadline.time_since_epoch());
    TEST_ASSERT_TRUE(c);
    TEST_ASSERT_EQUAL(12'000ms, c.value().deadline.time_since_epoch());
    TEST_ASSERT_TRUE(d);
    TEST_ASSERT_EQUAL(12'000ms, d.value().deadline.time_since_epoch());
    a.reset();
    b.reset();
    c.reset();
    d.reset();
    // Ensure the deadline is cleared on those events that are canceled.
    TEST_ASSERT(evt_a->getDeadline());
    TEST_ASSERT(evt_b->getDeadline());
    TEST_ASSERT(!evt_c->getDeadline());
    TEST_ASSERT(!evt_d->getDeadline());

    // Drop the second event and ensure it is removed from the tree immediately.
    SteadyClockMock::advance(1050ms);
    TEST_ASSERT_EQUAL(13'050ms, SteadyClockMock::now().time_since_epoch());
    TEST_ASSERT(evt_b->getDeadline());
    evt_b->cancel();
    TEST_ASSERT(!evt_b->getDeadline());                  // Unregistered, cleared.
    TEST_ASSERT_EQUAL_UINT64(1, evl->getTree().size());  // Freed already.
    evt_b->cancel();                                     // Idempotency.
    TEST_ASSERT(!evt_b->getDeadline());                  // Ditto.
    TEST_ASSERT_EQUAL_UINT64(1, evl->getTree().size());  // Ditto.
    out = evl->spin();
    // semihost::log(__LINE__, " ", out);
    TEST_ASSERT_EQUAL(14'000ms, out.next_deadline.time_since_epoch());  // B removed so the next one is A.
    TEST_ASSERT_EQUAL(50ms, out.worst_lateness);
    TEST_ASSERT_EQUAL(14'000ms, evl->getTree()[0U]->getDeadline().value().time_since_epoch());
    TEST_ASSERT_EQUAL_UINT64(1, evl->getTree().size());  // Second dropped.
    TEST_ASSERT_TRUE(a);
    TEST_ASSERT_EQUAL(13'000ms, a.value().deadline.time_since_epoch());
    TEST_ASSERT_FALSE(b);
    TEST_ASSERT_FALSE(c);
    TEST_ASSERT_FALSE(d);
    a.reset();

    // Nothing to do yet.
    out = evl->spin();
    // semihost::log(__LINE__, " ", out);
    TEST_ASSERT_EQUAL(14'000ms, out.next_deadline.time_since_epoch());  // Same up.
    TEST_ASSERT_EQUAL(0ms, out.worst_lateness);
    TEST_ASSERT_FALSE(a);
    TEST_ASSERT_FALSE(b);
    TEST_ASSERT_FALSE(c);
    TEST_ASSERT_FALSE(d);

    // Ensure the memory is properly reclaimed and there have been no OOMs.
    // semihost::log("Alloc before dtors: ", platform::heap::getDiagnostics().allocated);
    evt_a.reset();
    evt_b.reset();
    evt_c.reset();
    evt_d.reset();
    evl.reset();  // The destructor would panic unless all events are destroyed.
    // semihost::log("Alloc after dtors: ", platform::heap::getDiagnostics().allocated);
*/
}
/*
TEST(TestEmbeddedScheduler, EventLoopTotalOrdering)
{
    using std::chrono_literals::operator""ms;
    SteadyClockMock::reset();
    EventLoop<SteadyClockMock> evl;
    std::uint8_t               a      = 0;
    std::uint8_t               b      = 0;
    std::uint8_t               c      = 0;
    const auto                 report = [&](const auto tp, const char* const letter) {
        (void) tp;
        (void) letter;
        // semihost::log(tp, " ", letter, "! a=", a, " b=", b, " c=", c);  //
    };
    const auto evt_a = evl.repeat(10ms, [&](auto tp) {
        report(tp, "A");
        a++;
        TEST_ASSERT(a > b);
        TEST_ASSERT(a > c);
    });
    TEST_ASSERT_NOT_NULL(evt_a);
    const auto evt_b = evl.repeat(10ms, [&](auto tp) {
        report(tp, "B");
        b++;
        TEST_ASSERT(b <= a);
        TEST_ASSERT(b > c);
    });
    TEST_ASSERT_NOT_NULL(evt_b);
    const auto evt_c = evl.repeat(10ms, [&](auto tp) {
        report(tp, "C");
        c++;
        TEST_ASSERT(c <= a);
        TEST_ASSERT(c <= b);
    });
    TEST_ASSERT_NOT_NULL(evt_c);
    SteadyClockMock::advance(50ms);
    (void) evl.spin();
    TEST_ASSERT_EQUAL_INT64(5, a);
    TEST_ASSERT_EQUAL_INT64(5, b);
    TEST_ASSERT_EQUAL_INT64(5, c);
}

TEST(TestEmbeddedScheduler, EventLoopPoll)
{
    using time_point = SteadyClockMock::time_point;
    using std::chrono_literals::operator""ms;
    SteadyClockMock::reset();
    EventLoop<SteadyClockMock> evl;

    TEST_ASSERT_NULL(evl.poll(0ms, [&](auto) {}));  // Period shall be positive.
    TEST_ASSERT(evl.isEmpty());

    std::optional<time_point> last_tp{};
    SteadyClockMock::advance(100ms);
    auto evt = evl.poll(10ms, [&](const auto tp) {
        TEST_ASSERT_FALSE(last_tp);
        last_tp = tp.deadline;
    });
    TEST_ASSERT_NOT_NULL(evt);
    TEST_ASSERT_EQUAL(110ms, evl.getTree()[0U]->getDeadline().value().time_since_epoch());

    SteadyClockMock::advance(30ms);
    TEST_ASSERT_EQUAL(130ms, SteadyClockMock::now().time_since_epoch());
    (void) evl.spin();
    TEST_ASSERT(last_tp);
    TEST_ASSERT_EQUAL(110ms, last_tp.value().time_since_epoch());
    last_tp.reset();
    TEST_ASSERT_EQUAL(140ms, evl.getTree()[0U]->getDeadline().value().time_since_epoch());  // Skipped ahead!

    SteadyClockMock::advance(70ms);
    TEST_ASSERT_EQUAL(200ms, SteadyClockMock::now().time_since_epoch());
    (void) evl.spin();
    TEST_ASSERT(last_tp);
    TEST_ASSERT_EQUAL(140ms, last_tp.value().time_since_epoch());
    last_tp.reset();
    TEST_ASSERT_EQUAL(210ms, evl.getTree()[0U]->getDeadline().value().time_since_epoch());  // Skipped ahead!
}

TEST(TestEmbeddedScheduler, HandleMovement)
{
    using std::chrono_literals::operator""ms;

    SteadyClockMock::reset();

    EventLoop<SteadyClockMock> evl;

    auto a = evl.repeat(100ms, [&](auto) {});
    auto b = evl.repeat(103ms, [&](auto) {});
    auto c = evl.repeat(107ms, [&](auto) {});
    TEST_ASSERT_EQUAL_UINT64(3, evl.getTree().size());

    SteadyClockMock::advance(1000ms);
    TEST_ASSERT(a);
    a.reset();  // Destroy a
    TEST_ASSERT(!a);
    (void) evl.spin();
    TEST_ASSERT_EQUAL_UINT64(2, evl.getTree().size());

    TEST_ASSERT_NOT_NULL(b.get());
    auto d = std::move(b);  // b moved into d
    TEST_ASSERT_NOT_NULL(d.get());

    SteadyClockMock::advance(1000ms);
    (void) evl.spin();
    TEST_ASSERT_EQUAL_UINT64(2, evl.getTree().size());  // No change -- references moved but inferiors are kept alive.

    TEST_ASSERT_NOT_NULL(d.get());
    c = std::move(d);  // d moved into c, c destroyed.
    TEST_ASSERT_NOT_NULL(c.get());

    SteadyClockMock::advance(1000ms);
    (void) evl.spin();
    TEST_ASSERT_EQUAL_UINT64(1, evl.getTree().size());  // c destroyed, only b left alive (now in c).

    c.reset();
    SteadyClockMock::advance(1000ms);
    (void) evl.spin();
    TEST_ASSERT_EQUAL_UINT64(0, evl.getTree().size());
}
*/
}  // namespace embedded_scheduler::verification

// NOLINTEND(cppcoreguidelines-avoid-magic-numbers, readability-magic-numbers)
// NOLINTEND(readability-function-cognitive-complexity, misc-const-correctness)
