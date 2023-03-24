#include <type_traits>
#include <iostream>
#include <vector>
#include <functional>

namespace detail {
    template<typename R, typename = void>
    struct is_range : std::false_type {};

    template<typename R>
    struct is_range<R, std::void_t<decltype(std::declval<R>().begin()), decltype(std::declval<R>().end())>>
        : std::true_type {};


    template<typename R>
    static constexpr auto is_range_v = is_range<R>::value;


    template<typename V, typename = void>
    struct is_view : std::false_type {};

    template<typename V>
    struct is_view<V, std::void_t<std::enable_if_t<is_range_v<V>>, decltype(std::declval<V>().range())>>
        : std::true_type {};
        
    template<typename V>
    static constexpr auto is_view_v = is_view<V>::value;
}

template<typename Derived>
class base_view {
public:
};

template<typename R>
class all_view : public base_view<all_view<R>> {
    R const* m_pRange{};
    typename R::iterator m_begin, m_end;

    using base_t = base_view<all_view<R>>;
public:
    using value_type = typename R::value_type;
    using iterator = typename R::iterator;

    template<typename T>
    all_view(T&& r) noexcept : m_pRange{ &r }, m_begin{ r.begin() }, m_end{ r.end() } {
        static_assert(detail::is_range_v<std::remove_cv_t<std::remove_reference_t<T>>>, "Passed in type must be range");
    }

    auto begin() const { return m_begin; }
    auto end() const { return m_end; }
    auto range() const { return m_pRange; }
};

template<typename R>
all_view(R) -> all_view<R>;


template<typename View>
class filter_view {
public:
    class iterator;
    using pred_t = std::function<bool(typename View::value_type)>;

private:
    View m_view{};
    iterator m_begin, m_end;
    pred_t m_pred;

public:
    class iterator {
    public: // <-- important
        using difference_type = typename View::iterator::difference_type;
        using value_type = typename View::iterator::value_type;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category	= std::forward_iterator_tag;

    private:
        typename View::iterator m_iter{};
        filter_view const& m_filter_view;

    public:
        //constexpr iterator() = default;
        constexpr iterator(typename View::iterator iter, filter_view const& f) :  m_iter{ iter }, m_filter_view{ f } {
        }

        constexpr value_type operator*() const {  // <-- const important!
            return *m_iter;
        }

        constexpr iterator& operator++() {
            while (m_iter != m_filter_view.m_view.end()) {
                ++m_iter;
                if ((m_iter != m_filter_view.m_view.end()) and (m_filter_view.m_pred(*m_iter)))
                    break;
            };
            return *this;
        }

        constexpr iterator operator++(int) const {
            auto ret = *this;
            ++(*this);
            return ret;
        }

        // Must be friend
        friend constexpr bool operator==(iterator const& lhs, iterator const& rhs) noexcept {
            return lhs.m_iter == rhs.m_iter;
        }
        friend constexpr bool operator!=(iterator const& lhs, iterator const& rhs) noexcept {
            return not(lhs.m_iter == rhs.m_iter);
        }

        constexpr bool operator==(typename View::iterator rhs) const {
            return m_iter == rhs;
        }
    };

    template<typename View>
    constexpr filter_view(View&& v, pred_t f) noexcept : m_view{std::forward<View>(v)}, m_begin{ iterator{ v.begin(), *this } }, m_end{ iterator{ v.end(), *this } }, m_pred{ std::move(f) } {
        static_assert(detail::is_view_v<std::remove_cv_t<std::remove_reference_t<View>>>, "Must be a view");
        if (not(m_pred(*m_begin)))
            ++m_begin;
    }

    constexpr auto begin() const { return m_begin; }
    constexpr auto end() const { return m_end; }
    constexpr const auto& underlying() const { return m_view; }
};

template<typename V>
filter_view(V) -> filter_view<V>;


template<typename Pred>
struct filter_fn {
    Pred m_func{};

    template<typename P>
    filter_fn(P p) : m_func{ std::move(p) } {}

    template<typename R>
    constexpr auto operator()(R&& v) const {
        using stripped_t = std::remove_cv_t<std::remove_reference_t<R>>;
        return filter_view<all_view<stripped_t>>{ all_view<stripped_t>{ std::forward<R>(v) }, std::move(m_func) };
    }

    template<typename R>
    friend constexpr auto operator|(R&& r, filter_fn const& fn) {
        return fn(std::forward<R>(r));
    }
};

template<typename Pred>
auto filter(Pred p) {
    return filter_fn<Pred>{ std::move(p) };
}

int main() {
    std::vector v{ 1,2, 3,4,5,6,7,8,9 };
    //all_view allv{ v };
    //filter_view ff{ allv, [](int  i) { return i%2==0; } };

    for (auto i : v | filter([](int i) { return i % 2 == 0; })) {
        std::cout << i << "\n";
    }
}