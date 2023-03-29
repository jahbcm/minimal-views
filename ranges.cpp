#include <type_traits>
#include <iostream>
#include <vector>
#include <functional>

struct view_base {};

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
    struct is_view<V, std::enable_if_t<is_range_v<V> && std::is_base_of_v<view_base, V> && std::is_move_constructible_v<V>>>
        : std::true_type {};
        
    template<typename V>
    static constexpr auto is_view_v = is_view<V>::value;
}

/******************************************/
/* all_view */
/******************************************/
template<typename R, typename = void>
class all_view : public view_base {
    R const* m_pRange{};
    typename R::const_iterator m_begin, m_end;

public:
    using value_type = typename R::value_type;
    using iterator = typename R::const_iterator;
    using const_iterator = typename R::const_iterator;

    template<typename T>
    constexpr all_view(T&& r) noexcept : m_pRange{ &r }, m_begin{ m_pRange->cbegin() }, m_end{ m_pRange->cend() } {
        static_assert(detail::is_range_v<std::remove_cv_t<std::remove_reference_t<T>>>, "Passed in type must be range");
    }

    constexpr auto begin() const { return m_begin; }
    constexpr auto end() const { return m_end; }
    constexpr auto cbegin() const { return m_begin; }
    constexpr auto cend() const { return m_end; }
    constexpr auto size() const { return m_pRange->size(); }
    constexpr auto range() const { return m_pRange; }
};

template<typename V>
class all_view<V, std::enable_if_t<detail::is_view_v<V>>> : public view_base {
    V m_view{};
    typename V::iterator m_begin, m_end;

public:
    using value_type = typename V::value_type;
    using iterator = typename V::iterator;

    template<typename T, typename = std::enable_if_t<not std::is_same_v<std::remove_cv_t<std::remove_reference_t<T>>, all_view>>>
    constexpr all_view(T&& r) noexcept : m_view{ std::forward<T>(r) }, m_begin{ m_view.begin() }, m_end{ m_view.end() } {
    }

    constexpr all_view(all_view const& rhs)
        : m_view{rhs.m_view},
        m_begin{m_view.begin()},
        m_end{m_view.end()}
    {}
    constexpr all_view& operator=(all_view const& rhs) {
        all_view copy{ rhs };
        copy.swap(*this);
        return *this;
    }
    constexpr all_view(all_view&& rhs) noexcept
        : m_view{std::move(rhs.m_view)},
        m_begin{m_view.begin()},
        m_end{m_view.end()}
    {}
    constexpr all_view& operator=(all_view&& rhs) noexcept {
        all_view copy{ std::move(rhs) };
        copy.swap(*this); 
        return *this;
    }
    ~all_view() = default;

    friend constexpr void swap(all_view& lhs, all_view& rhs) noexcept {
        using std::swap;
        swap(lhs.m_view, rhs.m_view);
        swap(lhs.m_begin, rhs.m_begin);
        swap(lhs.m_end, rhs.m_end);
        lhs.m_begin.m_view = std::addressof(lhs.m_view);
        lhs.m_end.m_view = std::addressof(lhs.m_view);
        rhs.m_begin.m_view = std::addressof(rhs.m_view);
        rhs.m_end.m_view = std::addressof(rhs.m_view);
    }

    constexpr auto begin() const { return m_begin; }
    constexpr auto end() const { return m_end; }
    constexpr auto cbegin() const { return m_begin; }
    constexpr auto cend() const { return m_end; }
    constexpr const auto& underlying() const { return m_view; }
};

template<typename R>
all_view(R) -> all_view<R>;
/******************************************/

/******************************************/
/* filter_view                            */
/******************************************/
template<typename View>
class filter_view : public view_base {
public:
    class iterator;
    using pred_t = std::function<bool(typename View::value_type)>;
    using value_type = typename View::value_type;

private:
    View m_view{};
    pred_t m_pred;
    iterator m_begin, m_end;

public:
    class iterator {
    public: // <-- important
        using difference_type = typename View::iterator::difference_type;
        using value_type = typename View::iterator::value_type;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category	= std::forward_iterator_tag;

        filter_view* m_view{};
    private:
        typename View::iterator m_iter{};

    public:
        //constexpr iterator() = default;
        constexpr iterator(typename View::iterator iter, filter_view* f) :  m_iter{ iter }, m_view{ f } {
        }
        
        constexpr value_type operator*() const {  // <-- const important!
            return *m_iter;
        }

        constexpr iterator& operator++() {
            while (m_iter != m_view->m_view.end()) {
                ++m_iter;
                if ((m_iter != m_view->m_view.end()) and (m_view->m_pred(*m_iter)))
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
        constexpr bool operator!=(typename View::iterator rhs) const {
            return m_iter != rhs;
        }
    };

    using const_iterator = iterator;

    template<typename V, typename = std::enable_if_t<not std::is_same_v<std::remove_cv_t<std::remove_reference_t<V>>, filter_view>>>
    constexpr filter_view(V&& v, pred_t f) noexcept : 
        m_view{std::forward<V>(v)}, 
        m_pred{ std::move(f) },
        m_begin{ iterator{ m_view.begin(), this } }, 
        m_end{ iterator{ m_view.end(), this } } 
    {
        static_assert(detail::is_view_v<V>, "filter_view can only be constructed from a view!");
    }

    constexpr filter_view(filter_view const& rhs)
        : m_view{ rhs.m_view },
        m_pred{ rhs.m_pred },
        m_begin{ iterator{ m_view.begin(), this } },
        m_end{ iterator{ m_view.end(), this } }
    {}
    constexpr filter_view& operator=(filter_view const& rhs) {
        filter_view copy{ rhs };
        copy.swap(*this);
        return *this;
    }
    constexpr filter_view(filter_view&& rhs) noexcept :
        m_view{std::move(rhs.m_view)}, 
        m_pred{ std::move(rhs.m_pred) },
        m_begin{ iterator{ m_view.begin(), this } }, 
        m_end{ iterator{ m_view.end(), this } } 
    {
    }
    constexpr filter_view& operator=(filter_view&& rhs) noexcept {
        filter_view copy{ std::move(rhs) };
        copy.swap(*this);
        return *this;
    }
    ~filter_view() = default;

    friend constexpr void swap(filter_view& lhs, filter_view& rhs) noexcept {
        using std::swap;
        swap(lhs.m_view, rhs.m_view);
        swap(lhs.m_pred, rhs.m_pred);
        swap(lhs.m_begin, rhs.m_begin);
        swap(lhs.m_end, rhs.m_end);
        lhs.m_begin.m_view = std::addressof(lhs);
        lhs.m_end.m_view = std::addressof(lhs);
        rhs.m_begin.m_view = std::addressof(rhs);
        rhs.m_end.m_view = std::addressof(rhs);
    }

    constexpr auto begin() { return cbegin(); }
    constexpr auto end() const { return cend(); }
    constexpr auto cbegin() { 
        if ((m_begin != m_end) and not(m_pred(*m_begin)))
           ++m_begin;
        return m_begin;
    }
    constexpr auto cend() const { return m_end; }
    constexpr const auto& underlying() const { return m_view; }
};

template<typename V>
filter_view(V) -> filter_view<V>;


template<typename Pred>
struct filter_fn {
    Pred m_func{};

    template<typename P>
    constexpr filter_fn(P p) : m_func{ std::move(p) } {}

    template<typename R>
    constexpr auto operator()(R&& v) const {
        using stripped_t = std::remove_cv_t<std::remove_reference_t<R>>;
        return filter_view<all_view<stripped_t>>{ all_view<stripped_t>{ std::forward<R>(v) }, m_func };
    }

    template<typename R>
    friend constexpr auto operator|(R&& r, filter_fn const& fn) {
        return fn(std::forward<R>(r));
    }
};

template<typename Pred>
constexpr auto filter(Pred p) {
    return filter_fn<Pred>{ std::move(p) };
}
/******************************************/

/******************************************/
/* transform_view                         */
/******************************************/
template<typename View, typename ResultType>
class transform_view : public view_base {
public:
    class iterator;
    using transform_func_t = std::function<ResultType(typename View::value_type)>;
    using value_type = typename View::value_type;

private:
    View m_view{};
    transform_func_t m_func;
    iterator m_begin, m_end;

public:
    class iterator {
    public: // <-- important
        using difference_type = typename View::iterator::difference_type;
        using value_type = typename View::iterator::value_type;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category	= std::forward_iterator_tag;

        transform_view* m_view{};
    private:
        typename View::iterator m_iter{};

    public:
        //constexpr iterator() = default;
        constexpr iterator(typename View::iterator iter, transform_view* f) :  m_iter{ iter }, m_view{ f } {
        }
        
        constexpr ResultType operator*() const {  // <-- const important!
            return m_view->m_func(*m_iter);
        }

        constexpr iterator& operator++() {
            ++m_iter;
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
        constexpr bool operator!=(typename View::iterator rhs) const {
            return m_iter != rhs;
        }
    };

    using const_iterator = iterator;

    template<typename V, typename = std::enable_if_t<not std::is_same_v<std::remove_cv_t<std::remove_reference_t<V>>, transform_view>>>
    constexpr transform_view(V&& v, transform_func_t f) noexcept : 
        m_view{std::forward<V>(v)}, 
        m_func{ std::move(f) },
        m_begin{ iterator{ m_view.begin(), this } }, 
        m_end{ iterator{ m_view.end(), this } } 
    {
        static_assert(detail::is_view_v<V>, "transform_view can only be constructed from a view!");
    }

    constexpr transform_view(transform_view const& rhs)
        : m_view{ rhs.m_view },
        m_func{ rhs.m_func },
        m_begin{ iterator{ m_view.begin(), this } },
        m_end{ iterator{ m_view.end(), this } }
    {}
    constexpr transform_view& operator=(transform_view const& rhs) {
        transform_view copy{ rhs };
        copy.swap(*this);
        return *this;
    }
    constexpr transform_view(transform_view&& rhs) noexcept :
        m_view{std::move(rhs.m_view)}, 
        m_func{ std::move(rhs.m_func) },
        m_begin{ iterator{ m_view.begin(), this } }, 
        m_end{ iterator{ m_view.end(), this } } 
    {
    }
    constexpr transform_view& operator=(transform_view&& rhs) noexcept {
        transform_view copy{ std::move(rhs) };
        copy.swap(*this);
        return *this;
    }
    ~transform_view() = default;

    friend constexpr void swap(transform_view& lhs, transform_view& rhs) noexcept {
        using std::swap;
        swap(lhs.m_view, rhs.m_view);
        swap(lhs.m_func, rhs.m_func);
        swap(lhs.m_begin, rhs.m_begin);
        swap(lhs.m_end, rhs.m_end);
        lhs.m_begin.m_view = std::addressof(lhs);
        lhs.m_end.m_view = std::addressof(lhs);
        rhs.m_begin.m_view = std::addressof(rhs);
        rhs.m_end.m_view = std::addressof(rhs);
    }

    constexpr auto begin() const { return cbegin(); }
    constexpr auto end() const { return cend(); }
    constexpr auto cbegin() const { return m_begin; }
    constexpr auto cend() const { return m_end; }
    constexpr const auto& underlying() const { return m_view; }
};

template<typename V, typename F>
transform_view(V, F) -> transform_view<V, F>;


template<typename Func>
struct transform_fn {
    Func m_func{};

    template<typename F>
    constexpr transform_fn(F f) : m_func{ std::move(f) } {}

    template<typename View>
    constexpr auto operator()(View&& v) const {
        using stripped_t = std::remove_cv_t<std::remove_reference_t<View>>;
        return transform_view<all_view<stripped_t>, std::invoke_result_t<Func, View::value_type>>{ all_view<stripped_t>{ std::forward<View>(v) }, m_func };
    }

    template<typename R>
    friend constexpr auto operator|(R&& r, transform_fn const& fn) {
        return fn(std::forward<R>(r));
    }
};

template<typename Func>
constexpr auto transform(Func p) {
    return transform_fn<Func>{ std::move(p) };
}
/******************************************/


int main() {
    std::vector<int> v{ 1,2, 3,4,5,6,7,8,9,10,11,12,13,14,14,15,16,17,18,19,20 };
    //all_view allv{ v };
    //filter_view ff{ allv, [](int  i) { return i%2==0; } };

    auto ppp = v | 
        filter([](int i) { return i % 2 == 0; }) |
        filter([](int i) { return i > 10; }) |
        filter([](int i) { return i < 16; }) |
        filter([](int i) { return i == 14; }) |
        filter([](int i) { return i == 14; }) |
        transform([](int i) { return std::make_pair(i*2,i*2); });
    for (auto const& [i,j] : ppp) {
        std::cout << i << "," << j <<"\n";
    }
}