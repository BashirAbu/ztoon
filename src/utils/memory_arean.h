#pragma once
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <new>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
struct IArenaMember
{
    virtual ~IArenaMember() {}
    virtual void CallDestructor() = 0;
};

template <typename T>

struct ArenaMember : IArenaMember
{
    ArenaMember(T *ptr) : ptr(ptr){};
    void CallDestructor() override { ptr->~T(); }
    T *ptr;
};

class MemoryArena
{
  public:
    MemoryArena(size_t size) : size(size)
    {
        base_ptr = new uint8_t[size];
        memset(base_ptr, 0, size);
        head_ptr = base_ptr;
    }
    ~MemoryArena()
    {
        for (IArenaMember *member : arenaMembers)
        {
            member->CallDestructor();
            delete member;
        }

        delete[] base_ptr;
    }

    template <typename T, typename... Args> T *Allocate(Args... args)
    {
        size_t allocSize = sizeof(T);
        size_t alignment = alignof(T);

        size_t ptr = (size_t)(head_ptr);
        size_t padding = -ptr & (alignment - 1);

        assert(size >= GetAllocatedSize() + allocSize + padding);

        uint8_t *allocationPos = head_ptr + padding;
        head_ptr += padding;
        head_ptr += allocSize;
        T *allocated = new (allocationPos) T(args...);
        IArenaMember *member = new ArenaMember<T>(allocated);

        arenaMembers.push_back(member);

        allocatedSize += (allocSize + padding);

        return allocated;
    }

    size_t GetAllocatedSize() { return allocatedSize; }

  private:
    size_t size;
    uint8_t *head_ptr;
    uint8_t *base_ptr;
    size_t allocatedSize = 0;
    std::vector<IArenaMember *> arenaMembers;
};
