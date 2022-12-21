#ifndef KVFIFO_H
#define KVFIFO_H

#include <utility>
#include <list>
#include <utility>
#include <map>
#include <iostream>
#include <memory>

template<typename K, typename V>
class kvfifo {
    private:
        using listIterator = typename std::list<std::pair<K, V>>::iterator;
        using mapaT = typename std::map<K, std::list<listIterator>>;
        std::shared_ptr<std::list<std::pair<K, V>>> elements;
        std::shared_ptr<std::map<K, std::list<listIterator>>> keys;
        bool referenced = false;

        void makeCopy() {
            std::shared_ptr<std::list<std::pair<K, V>>> tempList;
            std::shared_ptr<std::map<K, std::list<listIterator>>> tempMap;
            bool fail = false;
            try {
                tempList = std::make_shared<std::list<std::pair<K, V>>>(*elements);
                tempMap = std::make_shared<std::map<K, std::list<listIterator>>>();
                listIterator it = (*tempList).begin();
                for (std::pair<K, V> element : (*tempList)) {
                    (*tempMap)[element.first].push_back(it);
                    it = next(it);
                }
            } catch (...) {
                fail = true;
            }
            if (!fail)
            {
                elements = tempList;
                keys = tempMap;
            }
        }

        void tryCopy() {
            if (!elements.unique())
            {
                makeCopy();
                referenced = false;
            }
        }

    public:
        class k_iterator : public mapaT::const_iterator {
            public:
                explicit k_iterator(typename mapaT::const_iterator it) : mapaT::const_iterator(it) {}
                K operator*() {
                    return mapaT::const_iterator::operator*().first;
                }
        };

        k_iterator k_begin() noexcept {
            return k_iterator(keys->cbegin());
        }

        k_iterator k_end() noexcept {
            return k_iterator(keys->cend());
        }

        kvfifo() {
            try {
                elements = std::make_shared<std::list<std::pair<K, V>>>();
                keys = std::make_shared<std::map<K, std::list<listIterator>>>();
            } catch (...) {
                elements = nullptr;
                keys = nullptr;
            }
        }

        kvfifo(kvfifo const &other) {
            elements = other.elements;
            keys = other.keys;
            if (other.referenced)
                makeCopy();
        }

        kvfifo(kvfifo &&other) noexcept {
            referenced = std::move(other.referenced);
            elements = std::move(other.elements);
            keys = std::move(other.keys);
        }

        kvfifo &operator=(kvfifo other) noexcept {
            if (elements == other.elements && keys == other.keys)
                return *this;

            elements = other.elements;
            keys = other.keys;
            if (other.referenced)
                makeCopy();

            referenced = false;
            return *this;
        }

        void push(K const &k, V const &v) {
            tryCopy();
            (*elements).push_back({k, v});
            listIterator it = prev((*elements).end());
            try {
                (*keys)[k].push_back(it);
            } catch (...) {
                (*elements).erase(it);
            }
        }

        void pop() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            tryCopy();

            std::pair<K, V> element = (*elements).front();
            (*elements).pop_front();
            (*keys)[element.first].pop_front();
        }

        void pop(K const &k) {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            tryCopy();

            listIterator it = (*keys)[k].front();
            (*keys)[k].pop_front();
            (*elements).erase(it);
        }

        void move_to_back(K const &k) {
            size_t size = count(k);
            if (size == 0)
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            typename std::list<listIterator>::iterator it = (*keys)[k].begin();
                for (size_t i = 0; i < size; ++i) {
                    auto temp = std::move(**it);
                    (*elements).erase(*it);
                    (*elements).push_back(std::move(temp));
                    *it = prev((*elements).end());
                    it = next(it);
                }
        }

        std::pair<K const &, V &> front() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            referenced = true;
            return {(*elements).front().first, (*elements).front().second};
        }

        std::pair<K const &, V const &> front() const {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            return {(*elements).front().first, (*elements).front().second};
        }

        std::pair<K const &, V &> back() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            referenced = true;
            return {(*elements).back().first, (*elements).back().second};
        }

        std::pair<K const &, V const &> back() const {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            return {(*elements).back().first, (*elements).back().second};
        }

        std::pair<K const &, V &> first(K const &k) {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            referenced = true;
            return {(*((*keys)[k].front())).first, (*((*keys)[k].front())).second};
        }

        std::pair<K const &, V const &> first(K const &k) const {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            return {(*((*keys)[k].front())).first, (*((*keys)[k].front())).second};
        }

        std::pair<K const &, V &> last(K const &k) {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            referenced = true;
            return {(*((*keys)[k].back())).first, (*((*keys)[k].back())).second};
        }

        std::pair<K const &, V const &> last(K const &k) const {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            return {(*((*keys)[k].back())).first, (*((*keys)[k].back())).second};
        }

        size_t size() const noexcept {
            return (*elements).size();
        }

        bool empty() const noexcept {
            return (*elements).empty();
        }

        size_t count(K const &x) const noexcept {
            if (!(*keys).contains(x))
                return 0;
            return (*keys)[x].size();
        }

        void clear() {
            elements = std::make_shared<std::list<std::pair<K, V>>>();
            keys = std::make_shared<std::map<K, std::list<listIterator>>>();
        }
};
#endif
