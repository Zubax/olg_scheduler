// Unauthorized copying and distribution of this file via any medium is strictly prohibited.
// Proprietary and confidential.
// Copyright (c) 2021  Zubax Robotics  <info@zubax.com>

#pragma once

#include <cstdint>
#include <memory>

namespace platform::heap
{
/// These functions are constant-complexity and adhere to the half-fit worst-case fragmentation model.
/// This free store shall only be used for small objects (typ. under 1 KiB) to avoid raising the Robson limit
/// and mitigate the risk of fragmentation.
/// For the related theory, see https://github.com/pavel-kirienko/o1heap.
inline void* allocate(const std::size_t amount) noexcept
{
    return std::malloc(amount);
}
inline void deallocate(void* const pointer) noexcept
{
    std::free(pointer);
}

/// Call destructor (unless nullptr) and then deallocate().
/// Direct usage is not recommended; use smart pointers instead.
template <typename T>
inline void destroy(T* const obj)
{
    static_assert((sizeof(T) > 0) && (!std::is_void_v<T>), "incomplete type");  // NOLINT(bugprone-sizeof-expression)
    if (obj != nullptr)
    {
        obj->~T();
    }
    deallocate(obj);
}

/// This is a no-op helper wrapper over destroy() needed for delayed function template instantiation.
struct Destroyer final
{
    template <typename T>
    void operator()(T* const obj)
    {
        destroy(obj);
    }
};

/// Simple unique pointer that automatically calls destroy().
template <typename T>
class UniquePtr final : public std::unique_ptr<T, Destroyer>
{
    using P = std::unique_ptr<T, Destroyer>;

public:
    using typename P::pointer;
    using typename P::element_type;

             UniquePtr() noexcept : P(nullptr, {}) {}
    explicit UniquePtr(T* const p) noexcept : P(p, {}) {}
    // NOLINTNEXTLINE(google-explicit-constructor,hicpp-explicit-conversions)
    UniquePtr(std::nullptr_t) noexcept : P(nullptr) {}
    template <typename U,
              typename = std::enable_if_t<std::is_convertible_v<typename UniquePtr<U>::pointer, typename P::pointer> &&
                                          !std::is_array_v<U>>>
    // NOLINTNEXTLINE(google-explicit-constructor,hicpp-explicit-conversions)
    UniquePtr(UniquePtr<U>&& other) noexcept : UniquePtr(other.release())
    {}

               UniquePtr(UniquePtr&&) noexcept = default;
    UniquePtr& operator=(UniquePtr&&) noexcept = default;

    ~UniquePtr() noexcept = default;

               UniquePtr(const UniquePtr&) = delete;
    UniquePtr& operator=(const UniquePtr&) = delete;
};

/// Simple object construction helper that allocates memory and calls placement new on it with the specified args.
/// Returns unique pointer which is nullptr if OOM.
template <typename T, typename... CtorArgs>
[[nodiscard]] inline UniquePtr<T> construct(CtorArgs&&... args)
{
    if (void* const mem = allocate(sizeof(T)))
    {
        return UniquePtr<T>(new (mem) T(std::forward<CtorArgs>(args)...));
    }
    return UniquePtr<T>();
}

/// This diagnostics info can be queried by the application to diagnose heap utilization.
struct Diagnostics
{
    std::size_t   capacity;           ///< Total heap capacity, this one is constant.
    std::size_t   allocated;          ///< Memory in use at the moment, including all overheads.
    std::size_t   peak_allocated;     ///< Largest seen value of the above since initialization.
    std::size_t   peak_request_size;  ///< Largest seen argument to allocate() since initialization.
    std::uint64_t oom_count;          ///< How many allocation requests were denied due to lack of memory.
};
Diagnostics getDiagnostics() noexcept;

}  // namespace platform::heap
