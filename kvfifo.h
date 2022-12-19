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
        using listIt = typename std::list<std::pair<K, V>>::iterator;
        std::shared_ptr<std::list<std::pair<K, V>>> elements;
        std::shared_ptr<std::map<K, std::list<listIt>>> keys;
    public:
        kvfifo() : elements(std::make_shared<std::list<std::pair<K, V>>>()), keys(std::make_shared<std::map<K, std::list<listIt>>>()) { }

        // jezeli ktos juz kopiuje po other to robie deep copy
        // jak oddam referencje to nie moze byc robiona kopia plytka
        kvfifo(kvfifo const &other) {
            elements = other.elements;
            keys = other.keys;
        }

        kvfifo(kvfifo &&other) noexcept : elements(std::move(other.elements)), keys(std::move(other.keys)) {}

        kvfifo &operator=(kvfifo other) {
            elements = other.elements;
            keys = other.keys;
            return *this;
        }

        void push(K const &k, V const &v) {
            elements->push_back({k, v});
            auto it = elements->end();
            --it;
            (*keys)[k].push_back(it);
        }

        void pop() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");

            std::pair<K, V> element = elements->front();
            elements->pop_front();
            (*keys)[element.first].pop_front();
        }

        void pop(K const &k) {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            if ((*keys)[k].empty())
                throw std::invalid_argument("Invalid operation.");

            listIt it = (*keys)[k].front();
            (*keys)[k].pop_front();
            elements->erase(it);
        }

        void move_to_back(K const &k) {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            std::list<listIt> keyIterators = (*keys)[k];
            if (keyIterators.empty())
                throw std::invalid_argument("Invalid operation.");

            auto it = elements->begin();
            while (it != keyIterators.end()) {
                push((**it).first, (**it).second);
                elements->erase(*it);
                it = keyIterators.erase(it);
            }
        }

        std::pair<K const &, V &> front() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            auto a = elements->front();
            return std::make_pair(std::cref(a.first), std::ref(a.second));
        }

        std::pair<K const &, V const &> front() const {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            auto a = elements->front();
            return std::make_pair(std::cref(a.first), std::cref(a.second));
        }

        std::pair<K const &, V &> back() {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            auto a = elements->back();
            return std::make_pair(std::cref(a.first), std::ref(a.second));
        }

        std::pair<K const &, V const &> back() const {
            if (empty())
                throw std::invalid_argument("Invalid operation.");
            auto a = elements->back();
            return std::make_pair(std::cref(a.first), std::cref(a.second));
        }

        std::pair<K const &, V &> first(K const &key) {
//            if (empty())
//                throw std::invalid_argument("Invalid operation.");
//            std::list<listIt> keyIterators = (*keys)[key];
//            if (keyIterators.empty())
//                throw std::invalid_argument("Invalid operation.");
//            listIt it = keyIterators.front();
//            return std::make_pair(std::cref(it->first), std::cref(it->second));
        }

        std::pair<K const &, V const &> first(K const &key) const {

        }

        std::pair<K const &, V &> last(K const &key) {

        }

        std::pair<K const &, V const &> last(K const &key) const {

        }

        size_t size() const {
            return elements->size();
        }

        bool empty() const {
            return elements->empty();
        }

        size_t count(K const &x) const {
            return (*keys)[x].size();
        }

        void clear() {
            elements = std::make_shared<std::list<std::pair<K, V>>>();
            keys = std::make_shared<std::map<K, std::list<listIt>>>();
        }
};
#endif