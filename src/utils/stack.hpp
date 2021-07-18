// copy of STL std::stack with begin/end iterators

#pragma once

#include <stack>

namespace shine::util {
    template<typename _Tp, typename _Sequence = std::deque<_Tp>>
    class stack {
    public:
        typedef typename _Sequence::value_type value_type;
        typedef typename _Sequence::reference reference;
        typedef typename _Sequence::const_reference const_reference;
        typedef typename _Sequence::iterator iterator;
        typedef typename _Sequence::const_iterator const_iterator;
        typedef typename _Sequence::const_reverse_iterator const_reverse_iterator;
        typedef typename _Sequence::reverse_iterator reverse_iterator;
        typedef typename _Sequence::size_type size_type;
        typedef _Sequence container_type;

    protected:
        _Sequence c;
        template<typename _Alloc>
        using _Uses = typename
        std::enable_if<std::uses_allocator<_Sequence, _Alloc>::value>::type;
    public:

        template<typename _Seq = _Sequence, typename _Requires = typename
        std::enable_if<std::is_default_constructible<_Seq>::value>::type>
        stack() : c() {}

        explicit stack(const _Sequence &__c) : c(__c) {}

        explicit stack(_Sequence &&__c) : c(std::move(__c)) {}

        template<typename _Alloc, typename _Requires = _Uses<_Alloc>>
        explicit stack(const _Alloc &__a): c(__a) {}

        template<typename _Alloc, typename _Requires = _Uses<_Alloc>>
        stack(const _Sequence &__c, const _Alloc &__a): c(__c, __a) {}

        template<typename _Alloc, typename _Requires = _Uses<_Alloc>>
        stack(_Sequence &&__c, const _Alloc &__a): c(std::move(__c), __a) {}

        template<typename _Alloc, typename _Requires = _Uses<_Alloc>>
        stack(const stack &__q, const _Alloc &__a): c(__q.c, __a) {}

        template<typename _Alloc, typename _Requires = _Uses<_Alloc>>
        stack(stack &&__q, const _Alloc &__a): c(std::move(__q.c), __a) {}

        value_type pop() {
            auto v = top();
            vpop();
            return v;
        }

        void vpop() {
            c.pop_back();
        }

        const_reference top() const { return c.back(); }

        reference top() { return c.back(); }

        iterator begin() { return c.begin(); }

        iterator end() { return c.end(); }

        const_iterator begin() const { return c.begin(); }

        const_iterator end() const { return c.end(); }

        reverse_iterator rbegin() { return c.rbegin(); }

        reverse_iterator rend() { return c.rend(); }

        const_reverse_iterator rbegin() const { return c.rbegin(); }

        const_reverse_iterator rend() const { return c.rend(); }

        bool empty() const { return c.empty(); }

        size_type size() const { return c.size(); }


        void push(const value_type &__x) { c.push_back(__x); }

        void push(value_type &&__x) { c.push_back(std::move(__x)); }

        template<typename... _Args>
        decltype(auto) emplace(_Args &&... __args) { return c.emplace_back(std::forward<_Args>(__args)...); }

        void swap(stack &__s) {
            using std::swap;
            swap(c, __s.c);
        }
    };
}
