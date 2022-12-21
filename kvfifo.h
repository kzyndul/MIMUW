#ifndef KVFIFO_H
#define KVFIFO_H

#include <utility>
#include <list>
#include <utility>
#include <map>
#include <iostream>
#include <memory>

template<typename K, typename V>
class k vfifo {
    private:
        using listIterator = typename std::list<std::pair<K, V>>::iterator;
        std::shared_ptr<std::list<std::pair<K, V>>> elements;
        std::shared_ptr<std::map<K, std::list<listIterator>>> keys;
        bool referenced;

        void makeCopy() {
            elements = std::make_shared<std::list<std::pair<K, V>>>(*elements);
            keys = std::make_shared<std::map<K, std::list<listIterator>>>();
            listIterator it = (*elements).begin();
            for (std::pair<K, V> element : (*elements)) {
                (*keys)[element.first].push_back(it);
                it = next(it);
            }
        }

        void tryCopy() {
            if (!elements.unique())
                makeCopy();
        }

    public:
        kvfifo() noexcept :
            elements(std::make_shared<std::list<std::pair<K, V>>>()),
            keys(std::make_shared<std::map<K, std::list<listIterator>>>()) {}


        kvfifo(kvfifo const &other) noexcept {
            /*elements = other.elements;
            keys = other.keys;
            if (other.referenced)
                makeCopy();*/
        }

        kvfifo(kvfifo &&other) noexcept : 
            elements(std::move(other.elements)), 
            keys(std::move(other.keys)) {}

        kvfifo &operator=(kvfifo other) {
            elements = other.elements;
            keys = other.keys;
            return *this;
        }

        void push(K const &k, V const &v) {
            (*elements).push_back({k, v});
            listIterator it = prev((*elements).end());
            (*keys)[k].push_back(it);
        }

        void pop() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");

            std::pair<K, V> element = (*elements).front();
            (*elements).pop_front();
            (*keys)[element.first].pop_front();
        }

        void pop(K const &k) {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");

            listIterator it = (*keys)[k].front();
            (*keys)[k].pop_front();
            (*elements).erase(it);
        }

        void move_to_back(K const &k) {
            size_t size = count(k);
            if (!size)
                throw std::invalid_argument("Invalid operation.");

            typename std::list<listIterator>::iterator it = (*keys)[k].begin();
            for (size_t i = 0; i < size; ++i) {
                (*elements).push_back(**it);
                (*elements).erase(*it);
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
            return {*(*keys)[k].front().first, *(*keys)[k].front().second};
        }

        std::pair<K const &, V const &> first(K const &k) const {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            return {*(*keys)[k].front().first, *(*keys)[k].front().second};
        }

        std::pair<K const &, V &> last(K const &k) {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            tryCopy();
            referenced = true;
            return {*(*keys)[k].back().first, *(*keys)[k].back().second};
        }

        std::pair<K const &, V const &> last(K const &k) const {
            if (!count(k))
                throw std::invalid_argument("Invalid operation.");
            return {*(*keys)[k].back().first, *(*keys)[k].back().second};
        }

        size_t size() const {
            return (*elements).size();
        }

        bool empty() const {
            return (*elements).empty();
        }

        size_t count(K const &x) const {
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
