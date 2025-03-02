#pragma once
#include <algorithm>
#include <cstddef>
#include <functional>
#include <list>
#include <string>
#include <utility>
#include <vector>
template <typename Key, typename Value>

class HashMap
{
  public:
    HashMap() : buckets(16) {}
    ~HashMap() {}

    bool contains(Key key)
    {
        for (auto &kv : buckets[GetIndex(key)])
        {
            if (kv.first == key)
            {
                return true;
            }
        }
        return false;
    }

    Value &operator[](const Key &key)
    {
        size_t index = GetIndex(key);

        for (auto &kv : buckets[index])
        {
            if (kv.first == key)
            {
                return kv.second;
            }
        }

        // add
        buckets[index].push_back({key, Value()});
        return buckets[index].back().second;
    }

  private:
    size_t GetIndex(const Key &key)
    {
        auto h = hash(key);
        size_t s = buckets.size();
        return s == 0 ? 0 : h % s;
    }

    std::hash<Key> hash;
    std::vector<std::list<std::pair<Key, Value>>> buckets;
};
