#include <type_traits>
#include <iostream>
#include <vector>
#include <functional>
#include <optional>

#if _MSC_VER < 1920
#define PRS_VIEWS_CONSTEXPR
#else
#define PRS_VIEWS_CONSTEXPR constexpr
#endif

struct view_base {};

namespace detail {
    template<typename R, typename = void>
    struct is_range : std::false_type {};

    /* A range requires the presence of begin() and end() */
    template<typename R>
    struct is_range<R, std::void_t<decltype(std::declval<R>().begin()), decltype(std::declval<R>().end())>>
        : std::true_type {};


    template<typename R>
    static constexpr auto is_range_v = is_range<R>::value;


    template<typename V, typename = void>
    struct is_view : std::false_type {};

    /* 
        A view must:
        - be a range
        - be derived from view_base
        - be move constructible
        - have the member types iterator and value_type
    */
    template<typename V>
    struct is_view<V, std::enable_if_t<
        is_range<V>::value &&
        std::is_base_of_v<view_base, V> &&
        std::is_move_constructible_v<V>
    , std::void_t<typename V::iterator, typename V::value_type>>>
        : std::true_type {};

    template<typename V>
    static constexpr auto is_view_v = is_view<V>::value;
}

/******************************************/
/* all_view								  */
/*										  */
/* Bottom-level view that wraps a non-view*/
/* range like std::vector and can then    */
/* be used with other views (which all	  */
/* require views as input				  */			
/******************************************/
template<typename R, typename = void>
class all_view : public view_base {
public:
    using iterator = std::conditional_t<std::is_const_v<R>, typename R::const_iterator, typename R::iterator>;
private:
    R* m_pRange{};
    iterator m_begin, m_end;

public:
    using value_type = std::conditional_t<std::is_const_v<R>, std::add_const_t<typename R::value_type>, typename R::value_type>;

    template<typename T>
    constexpr all_view(T&& r) noexcept : m_pRange{ &r }, m_begin{
        [this]() {
            if constexpr (std::is_const_v<R>)
                return m_pRange->cbegin();
            else
                return m_pRange->begin();
        }()
    }, m_end{
        [this]() {
            if constexpr (std::is_const_v<R>)
                return m_pRange->cend();
            else
                return m_pRange->end();
        }()
    }
    {
        static_assert(detail::is_range_v<std::remove_cv_t<std::remove_reference_t<T>>>, "Passed in type must be range");
    }

    constexpr auto begin() const { return m_begin; }
    constexpr auto end() const { return m_end; }
    constexpr auto size() const { return m_pRange->size(); }
    constexpr auto range() const { return m_pRange; }
};

/******************************************/
/* all_view								  */
/*										  */
/* When a view is constructed by its      */
/* corresponding _fn function object,     */
/* this overload or the previous one will */
/* be chosen to wrap what is passed.	  */
/* Technically this wrapping is only	  */
/* once at the container->view interface  */
/* but right now I'm not checking and	  */
/* always wrap in an all_view, regardless */
/* of whether it already is one.		  */	
/******************************************/
template<typename V>
class all_view<V, std::enable_if_t<detail::is_view_v<V>>> : public view_base {
    V m_view{};
    typename V::iterator m_begin, m_end;

public:
    using value_type = typename V::value_type;
    using iterator = typename V::iterator;

    template<typename T, typename = std::enable_if_t<!std::is_same_v<std::remove_cv_t<std::remove_reference_t<T>>, all_view>>>
    constexpr all_view(T&& r) noexcept : m_view{ std::forward<T>(r) }, m_begin{ m_view.begin() }, m_end{ m_view.end() } {
    }

    constexpr all_view(all_view const& rhs)
        : m_view{ rhs.m_view },
        m_begin{ m_view.begin() },
        m_end{ m_view.end() }
    {}
    constexpr all_view& operator=(all_view const& rhs) {
        all_view copy{ rhs };
        copy.swap(*this);
        return *this;
    }
    constexpr all_view(all_view&& rhs) noexcept
        : m_view{ std::move(rhs.m_view) },
        m_begin{ m_view.begin() },
        m_end{ m_view.end() }
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
    constexpr const auto& underlying() const { return m_view; }
};

template<typename R>
all_view(R) -> all_view<R>;
/******************************************/

/******************************************/
/* filter_view                            */
/* Enables filtering a view, i.e. only    */
/* iterating the elements that match a	  */
/* certain predicate.					  */	
/******************************************/
template<typename View>
class filter_view : public view_base {
public:
    class iterator;
    using pred_t = std::function<bool(typename View::value_type&)>;
    using value_type = typename View::value_type;

private:
    View m_view{};
    pred_t m_pred;
    std::optional<iterator> m_begin;
    iterator m_end;

public:
    class iterator {
    public: // <-- important
        using difference_type = typename View::iterator::difference_type;
        using value_type = typename View::value_type;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category = std::forward_iterator_tag;

        filter_view* m_view{};
    private:
        typename View::iterator m_iter{};

    public:
        //constexpr iterator() = default;
        constexpr iterator(typename View::iterator iter, filter_view* f) : m_iter{ iter }, m_view{ f } {
        }

        constexpr reference operator*() const {  // <-- const important!
            return *m_iter;
        }

        constexpr iterator& operator++() {
            while (m_iter != m_view->m_view.end()) {
                ++m_iter;
                if ((m_iter != m_view->m_view.end()) && (m_view->m_pred(*m_iter)))
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
        friend PRS_VIEWS_CONSTEXPR bool operator==(iterator const& lhs, iterator const& rhs) noexcept {
            return lhs.m_iter == rhs.m_iter;
        }
        friend PRS_VIEWS_CONSTEXPR bool operator!=(iterator const& lhs, iterator const& rhs) noexcept {
            return !(lhs.m_iter == rhs.m_iter);
        }

        constexpr bool operator==(typename View::iterator rhs) const {
            return m_iter == rhs;
        }
        constexpr bool operator!=(typename View::iterator rhs) const {
            return m_iter != rhs;
        }
    };

    template<typename V, typename = std::enable_if_t<!std::is_same_v<std::remove_cv_t<std::remove_reference_t<V>>, filter_view>>>
    constexpr filter_view(V&& v, pred_t f) noexcept :
        m_view{ std::forward<V>(v) },
        m_pred{ std::move(f) },
        m_end{ iterator{ m_view.end(), this } }
    {
        static_assert(detail::is_view_v<V>, "filter_view can only be constructed from a view!");
    }

    constexpr filter_view(filter_view const& rhs)
        : m_view{ rhs.m_view },
        m_pred{ rhs.m_pred },
        m_end{ iterator{ m_view.end(), this } }
    {}
    constexpr filter_view& operator=(filter_view const& rhs) {
        filter_view copy{ rhs };
        copy.swap(*this);
        return *this;
    }
    constexpr filter_view(filter_view&& rhs) noexcept :
        m_view{ std::move(rhs.m_view) },
        m_pred{ std::move(rhs.m_pred) },
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

    constexpr auto begin() {
        if (!m_begin.has_value()) {
            m_begin = iterator{ m_view.begin(), this };
            m_begin = std::find_if(m_begin.value(), m_end, std::cref(m_pred));
        }
        return m_begin.value();
    }
    constexpr auto end() const { return m_end; }
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
    constexpr auto operator()(R&& v) const& {
        using stripped_t = std::remove_reference_t<R>;
        return filter_view<all_view<stripped_t>>{ all_view<stripped_t>{ std::forward<R>(v) }, m_func };
    }
    template<typename R>
    constexpr auto operator()(R&& v) && {
        using stripped_t = std::remove_reference_t<R>;
        return filter_view<all_view<stripped_t>>{ all_view<stripped_t>{ std::forward<R>(v) }, std::move(m_func) };
    }

    template<typename R>
    friend constexpr auto operator|(R&& r, filter_fn const& fn) {
        return fn(std::forward<R>(r));
    }
    template<typename R>
    friend constexpr auto operator|(R&& r, filter_fn&& fn) {
        return std::move(fn)(std::forward<R>(r));
    }
};

template<typename Pred>
constexpr auto filter(Pred p) {
    return filter_fn<Pred>{ std::move(p) };
}
/******************************************/

/******************************************/
/* transform_view                         */
/* Iterates over all elements of a view   */
/* and performs the given transformation  */
/* on each element, thereby providing a	  */
/* transformed view of the input range    */
/******************************************/
template<typename View, typename ResultType>
class transform_view : public view_base {
public:
    class iterator;
    using transform_func_t = std::function<ResultType(typename View::value_type&)>;
    using value_type = typename View::value_type;

private:
    View m_view{};
    transform_func_t m_func;
    iterator m_begin, m_end;

public:
    class iterator {
    public: // <-- important
        using difference_type = typename View::iterator::difference_type;
        using value_type = typename View::value_type;
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
        friend PRS_VIEWS_CONSTEXPR bool operator==(iterator const& lhs, iterator const& rhs) noexcept {
            return lhs.m_iter == rhs.m_iter;
        }
        friend PRS_VIEWS_CONSTEXPR bool operator!=(iterator const& lhs, iterator const& rhs) noexcept {
            return !(lhs.m_iter == rhs.m_iter);
        }

        constexpr bool operator==(typename View::iterator rhs) const {
            return m_iter == rhs;
        }
        constexpr bool operator!=(typename View::iterator rhs) const {
            return m_iter != rhs;
        }
    };

    template<typename V, typename = std::enable_if_t<!std::is_same_v<std::remove_cv_t<std::remove_reference_t<V>>, transform_view>>>
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

    constexpr auto begin() const { return m_begin; }
    constexpr auto end() const { return m_end; }
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
    constexpr auto operator()(View&& v) const& {
        using stripped_t = std::remove_reference_t<View>;
        return transform_view<all_view<stripped_t>, std::invoke_result_t<Func, typename View::value_type&>>{ all_view<stripped_t>{ std::forward<View>(v) }, m_func };
    }
    template<typename View>
    constexpr auto operator()(View&& v) && {
        using stripped_t = std::remove_reference_t<View>;
        return transform_view<all_view<stripped_t>, std::invoke_result_t<Func, typename View::value_type&>>{ all_view<stripped_t>{ std::forward<View>(v) }, std::move(m_func) };
    }

    template<typename R>
    friend constexpr auto operator|(R&& r, transform_fn const& fn) {
        return fn(std::forward<R>(r));
    }
    template<typename R>
    friend constexpr auto operator|(R&& r, transform_fn&& fn) {
        return std::move(fn)(std::forward<R>(r));
    }
};

template<typename Func>
constexpr auto transform(Func p) {
    return transform_fn<Func>{ std::move(p) };
}
/******************************************/


/******************************************/
/* zip_view                               */
/* Enables simultaneous treatment of	  */
/* corresponding elements from different  */
/* views. Provides tuples of corresponding*/
/* elements.							  */	
/******************************************/
class zip_base {
public:
    struct sentinel {};
};
template<typename... Views>
class zip_view : public view_base, public zip_base {
public:
    class iterator;
    using value_type = std::tuple<typename Views::iterator::reference...>;

private:
    std::tuple<Views...> m_views{};
    iterator m_begin, m_end;

    template<bool MakeBegin, std::size_t... Is>
    constexpr auto _makeBeginOrEndIter(std::index_sequence<Is...>) {
        if constexpr (MakeBegin)
            return iterator{ std::make_tuple(std::get<Is>(m_views).begin()...), this };
        else
            return iterator{ std::make_tuple(std::get<Is>(m_views).end()...), this };
    }

    constexpr auto _makeBeginIter() {
        return _makeBeginOrEndIter<true>(std::index_sequence_for<Views...>{});
    }
    constexpr auto _makeEndIter() {
        return _makeBeginOrEndIter<false>(std::index_sequence_for<Views...>{});
    }

public:
    class iterator {
    public: // <-- important
        using difference_type = std::ptrdiff_t;
        using value_type = std::tuple<typename Views::iterator::reference...>;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category = std::forward_iterator_tag;

        zip_view* m_view{};
    private:
        using underlying_iterator = std::tuple<typename Views::iterator...>;
        underlying_iterator m_iter{};


        template<std::size_t... Is>
        constexpr auto _dereference(std::index_sequence<Is...>) const {
            return value_type{ *std::get<Is>(m_iter)... };
        }
        template<std::size_t... Is>
        constexpr void _advanceIter(std::index_sequence<Is...>) {
            (++std::get<Is>(m_iter), ...);
        }
        template<std::size_t... Is>
        constexpr auto _compareEqual(iterator const& rhs, std::index_sequence<Is...>) const {
            return ((std::get<Is>(m_iter) == std::get<Is>(rhs.m_iter)) && ...);
        }
        template<std::size_t... Is>
        constexpr auto _checkEnd(std::index_sequence<Is...>) const {
            return ((std::get<Is>(m_iter) == std::get<Is>(m_view->m_views).end()) || ...);
        }

    public:
        //constexpr iterator() = default;
        constexpr iterator(underlying_iterator iter, zip_view* f) : m_iter{ iter }, m_view{ f } {
        }

        constexpr value_type operator*() const {  // <-- const important!
            return _dereference(std::index_sequence_for<Views...>{});
        }

        constexpr iterator& operator++() {
            _advanceIter(std::index_sequence_for<Views...>{});
            return *this;
        }

        constexpr iterator operator++(int) const {
            auto ret = *this;
            ++(*this);
            return ret;
        }

        // Must be friend
        friend PRS_VIEWS_CONSTEXPR bool operator==(iterator const& lhs, iterator const& rhs) noexcept {
            return lhs._compareEqual(rhs, std::index_sequence_for<Views...>{});
        }
        friend PRS_VIEWS_CONSTEXPR bool operator!=(iterator const& lhs, iterator const& rhs) noexcept {
            return !(lhs.m_iter == rhs.m_iter);
        }

        constexpr auto operator!=(zip_base::sentinel) const noexcept {
            return !_checkEnd(std::index_sequence_for<Views...>{});
        }

        /*
        constexpr bool operator==(typename View::iterator rhs) const {
            return m_iter == rhs;
        }
        constexpr bool operator!=(typename View::iterator rhs) const {
            return m_iter != rhs;
        }
        */
    };

    template<typename... V>
    constexpr zip_view(V&&... v) noexcept :
        m_views{ std::forward<V>(v)... },
        m_begin{ _makeBeginIter() },
        m_end{ _makeEndIter() }
    {
        static_assert((detail::is_view_v<V> && ...), "zip_view can only be constructed from views!");
    }

    constexpr zip_view(zip_view const& rhs)
        : m_views{ rhs.m_views },
        m_begin{ _makeBeginIter() },
        m_end{ _makeEndIter() }
    {}
    constexpr zip_view& operator=(zip_view const& rhs) {
        zip_view copy{ rhs };
        copy.swap(*this);
        return *this;
    }
    constexpr zip_view(zip_view&& rhs) noexcept :
        m_views{ std::move(rhs.m_views) },
        m_begin{ _makeBeginIter() },
        m_end{ _makeEndIter() }
    {}
    constexpr zip_view& operator=(zip_view&& rhs) noexcept {
        zip_view copy{ std::move(rhs) };
        copy.swap(*this);
        return *this;
    }
    ~zip_view() = default;

    friend constexpr void swap(zip_view& lhs, zip_view& rhs) noexcept {
        using std::swap;
        swap(lhs.m_views, rhs.m_views);
        swap(lhs.m_begin, rhs.m_begin);
        swap(lhs.m_end, rhs.m_end);
        lhs.m_begin.m_view = std::addressof(lhs);
        lhs.m_end.m_view = std::addressof(lhs);
        rhs.m_begin.m_view = std::addressof(rhs);
        rhs.m_end.m_view = std::addressof(rhs);
    }

    constexpr auto begin() const { return m_begin; }
    constexpr auto end() const { return sentinel{}; }
    constexpr const auto& underlying() const { return m_views; }
};

template<typename... V>
zip_view(V...) -> zip_view<V...>;


struct zip_fn {
    template<typename... Views>
    constexpr auto operator()(Views&&... v) const {
        return zip_view<all_view<std::remove_reference_t<Views>>...>{ all_view<std::remove_reference_t<Views>>{ std::forward<Views>(v) }... };
    }

    /*
    * Seemingly pointless now
    *
        template<typename R>
        friend constexpr auto operator|(R&& r, zip_fn const& fn) {
            return fn(std::forward<R>(r));
        }
    */
};

static inline constexpr auto zip = zip_fn{};

/******************************************/

/******************************************/
/* transform_view                         */
/* Iterates over all elements of a view   */
/* and performs the given transformation  */
/* on each element, thereby providing a	  */
/* transformed view of the input range    */
/******************************************/
template<typename View>
class inverse_view : public view_base {
public:
    class iterator;
    //using inverse_func_t = std::function<ResultType(typename View::value_type&)>;
    using value_type = typename View::value_type;

private:
    View m_view{};
    //inverse_func_t m_func;
    iterator m_begin, m_end;

public:
    class iterator {
    public: // <-- important
        using difference_type = typename View::iterator::difference_type;
        using value_type = typename View::value_type;
        using pointer = value_type*;
        using reference = value_type&;
        using iterator_category	= std::forward_iterator_tag;

        inverse_view* m_view{};
    private:
        typename View::iterator m_iter{};

    public:
        //constexpr iterator() = default;
        constexpr iterator(typename View::iterator iter, inverse_view* f) :  m_iter{ iter }, m_view{ f } {
        }
        
        constexpr reference operator*() const {  // <-- const important!
            return *m_iter;
        }

        constexpr iterator& operator++() {
            --m_iter;
            return *this;
        }

        constexpr iterator operator++(int) const {
            auto ret = *this;
            --(*this);
            return ret;
        }

        // Must be friend
        friend PRS_VIEWS_CONSTEXPR bool operator==(iterator const& lhs, iterator const& rhs) noexcept {
            return lhs.m_iter == rhs.m_iter;
        }
        friend PRS_VIEWS_CONSTEXPR bool operator!=(iterator const& lhs, iterator const& rhs) noexcept {
            return !(lhs.m_iter == rhs.m_iter);
        }

        constexpr bool operator==(typename View::iterator rhs) const {
            return m_iter == rhs;
        }
        constexpr bool operator!=(typename View::iterator rhs) const {
            return m_iter != rhs;
        }
    };

    template<typename V, typename = std::enable_if_t<!std::is_same_v<std::remove_cv_t<std::remove_reference_t<V>>, inverse_view>>>
    constexpr inverse_view(V&& v) noexcept : 
        m_view{std::forward<V>(v)}, 
        //m_func{ std::move(f) },
        m_begin{ iterator{ --m_view.end(), this } }, 
        m_end{ iterator{ --m_view.begin(), this } } 
    {
        static_assert(detail::is_view_v<V>, "inverse_view can only be constructed from a view!");
    }

    constexpr inverse_view(inverse_view const& rhs)
        : m_view{ rhs.m_view },
        //m_func{ rhs.m_func },
        m_begin{ iterator{ --m_view.end(), this } },
        m_end{ iterator{ --m_view.begin(), this } }
    {}
    constexpr inverse_view& operator=(inverse_view const& rhs) {
        inverse_view copy{ rhs };
        copy.swap(*this);
        return *this;
    }
    constexpr inverse_view(inverse_view&& rhs) noexcept :
        m_view{std::move(rhs.m_view)}, 
        //m_func{ std::move(rhs.m_func) },
        m_begin{ iterator{ m_view.begin(), this } }, 
        m_end{ iterator{ m_view.end(), this } } 
    {
    }
    constexpr inverse_view& operator=(inverse_view&& rhs) noexcept {
        inverse_view copy{ std::move(rhs) };
        copy.swap(*this);
        return *this;
    }
    ~inverse_view() = default;

    friend constexpr void swap(inverse_view& lhs, inverse_view& rhs) noexcept {
        using std::swap;
        swap(lhs.m_view, rhs.m_view);
        //swap(lhs.m_func, rhs.m_func);
        swap(lhs.m_begin, rhs.m_begin);
        swap(lhs.m_end, rhs.m_end);
        lhs.m_begin.m_view = std::addressof(lhs);
        lhs.m_end.m_view = std::addressof(lhs);
        rhs.m_begin.m_view = std::addressof(rhs);
        rhs.m_end.m_view = std::addressof(rhs);
    }

    constexpr auto begin() const { return m_begin; }
    constexpr auto end() const { return m_end; }
    constexpr const auto& underlying() const { return m_view; }
};

template<typename V>
inverse_view(V) -> inverse_view<V>;


//template<typename Func>
struct inverse_fn {
    //Func m_func{};

    //template<typename F>
    constexpr inverse_fn() {}

    template<typename View>
    constexpr auto operator()(View&& v) const& {
        using stripped_t = std::remove_reference_t<View>;
        return inverse_view<all_view<stripped_t>>{ all_view<stripped_t>{ std::forward<View>(v) }};
    }
    template<typename View>
    constexpr auto operator()(View&& v) && {
        using stripped_t = std::remove_reference_t<View>;
        return inverse_view<all_view<stripped_t>>{ all_view<stripped_t>{ std::forward<View>(v) }};
    }

    template<typename R>
    friend constexpr auto operator|(R&& r, inverse_fn const& fn) {
        return fn(std::forward<R>(r));
    }
    template<typename R>
    friend constexpr auto operator|(R&& r, inverse_fn&& fn) {
        return std::move(fn)(std::forward<R>(r));
    }
};

//template<typename Func>
constexpr auto inverse() {
    return inverse_fn{ };
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