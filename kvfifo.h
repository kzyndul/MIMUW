#ifndef KVFIFO_H
#define KVFIFO_H

#include <utility>
#include <list>
#include <utility>
#include <map>
#include <iostream>
#include <memory>

template<typename K, typename V> requires std::totally_ordered<K> && std::regular<K> && std::copy_constructible<V>
class kvfifo {
    private:
        using listIterator = typename std::list<std::pair<K, V>>::iterator;
        using mapIterator = typename std::map<K, std::list<listIterator>>;
        std::shared_ptr<std::list<std::pair<K, V>>> elements;
        std::shared_ptr<std::map<K, std::list<listIterator>>> keys;
        bool referenced = false;

        void makeCopy() {
            std::shared_ptr<std::list<std::pair<K, V>>> oldElements = elements;
            std::shared_ptr<std::map<K, std::list<listIterator>>> oldKeys = keys;

             try {
                 elements = std::make_shared<std::list<std::pair<K, V>>>(*elements);
                 keys = std::make_shared<std::map<K, std::list<listIterator>>>();
                 listIterator it = (*elements).begin();
                 for (std::pair<K, V> element : (*elements)) {
                     (*keys)[element.first].push_back(it);
                     it = next(it);
                 }
                 referenced = false;
             } catch(...) {
                elements = oldElements;
                keys = oldKeys;
                throw;
             }
        }

        void tryCopy() {
            if (!elements.unique())
                makeCopy();
        }

        static std::shared_ptr<std::list<std::pair<K, V>>> emptyElements() {
            static const auto empty = std::make_shared<std::list<std::pair<K, V>>>();
            return empty;
        }

        static std::shared_ptr<std::map<K, std::list<listIterator>>> emptyKeys() {
            static const auto empty = std::make_shared<std::map<K, std::list<listIterator>>>();
            return empty;
        }

    public:
        class k_iterator : public mapIterator::const_iterator {
            public:
                explicit k_iterator(typename mapIterator::const_iterator it) : mapIterator::const_iterator(it) {}
                K operator*() {
                    return mapIterator::const_iterator::operator*().first;
                }
        };

        k_iterator k_begin() const noexcept {
            return k_iterator(keys->cbegin());
        }

        k_iterator k_end() const noexcept {
            return k_iterator(keys->cend());
        }

        kvfifo() :
            elements(std::make_shared<std::list<std::pair<K, V>>>()),
            keys(std::make_shared<std::map<K, std::list<listIterator>>>()),
            referenced(false) {}

        kvfifo(kvfifo const &other) :
            elements(other.elements),
            keys(other.keys),
            referenced(false) {
            if (other.referenced)
                makeCopy();
        }

        kvfifo(kvfifo &&other) noexcept :
                elements(std::move(other.elements)),
                keys(std::move(other.keys)),
                referenced(std::move(other.referenced)) {
            other.elements = emptyElements();
            other.keys = emptyKeys();
        }

        kvfifo &operator=(kvfifo other) {
            if (elements == other.elements && keys == other.keys)
                return *this;

            std::shared_ptr<std::list<std::pair<K, V>>> oldElements = elements;
            std::shared_ptr<std::map<K, std::list<listIterator>>> oldKeys = keys;
            try {
                elements = other.elements;
                keys = other.keys;
                referenced = false;
                if (other.referenced)
                    makeCopy();
                return *this;
            } catch (...) {
                elements = oldElements;
                keys = oldKeys;
                throw;
            }
        }

        void push(K const &k, V const &v) {
            tryCopy();
            (*elements).push_back({k, v});
            listIterator it = prev((*elements).end());
            try {
                (*keys)[k].push_back(it);
                referenced = false;
            } catch (...) {
                (*elements).erase(it);
                if (!count(k))
                    (*keys).erase(k);
                throw;
            }
        }

        void pop() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            std::pair<K, V> element = (*elements).front();
            (*elements).pop_front();
            (*keys)[element.first].pop_front();
            if (!count(element.first))
                (*keys).erase(element.first);
            referenced = false;
        }

        void pop(K const &k) {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            listIterator it = (*keys)[k].front();
            (*keys)[k].pop_front();
            (*elements).erase(it);
            if (!count(k))
                (*keys).erase(k);
            referenced = false;
        }

        void move_to_back(K const &k) {
            size_t size = count(k);
            if (!size)
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            for (auto it : (*keys)[k])
                (*elements).splice((*elements).end(), (*elements), it);
            referenced = false;
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
            if (!elements)
                return 0;
            return (*elements).size();
        }

        bool empty() const noexcept {
            if (!elements)
                return true;
            return (*elements).empty();
        }

        size_t count(K const &x) const noexcept {
            if (!keys || !(*keys).contains(x))
                return 0;
            return (*keys)[x].size();
        }

        void clear() {
            tryCopy();
            (*elements).clear();
            (*keys).clear();
        }
};
#endif
