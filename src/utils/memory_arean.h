#pragma once
#include <cassert>
#include <cstddef>
#include <cstdlib>
#include <new>
#include <vector>
struct IArenaMember
{
    virtual ~IArenaMember() {}
    virtual void CallDestructor() = 0;
};

template <typename T>

struct ArenaMember : IArenaMember
{
    ArenaMember(T *ptr) : ptr(ptr) {};
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

        delete base_ptr;
    }

    template <typename T, typename... Args> T *Allocate(Args... args)
    {
        size_t allocSize = sizeof(T);
        assert(size >= GetAllocatedSize() + allocSize);

        T *allocated = new (head_ptr) T(args...);
        head_ptr += allocSize;
        IArenaMember *member = new ArenaMember<T>(allocated);

        arenaMembers.push_back(member);
        return allocated;
    }
    template <typename T, typename... Args>
    T *AllocateArray(size_t arrSize, Args... args)
    {
        size_t allocSize = sizeof(T) * arrSize;
        assert(size >= (GetAllocatedSize() + allocSize));
        T *ret = (T *)base_ptr;
        for (size_t index = 0; index < (allocSize / (sizeof(T))); index++)
        {
            T *allocated = new (head_ptr) T(args...);
            head_ptr += sizeof(T);
            IArenaMember *member = new ArenaMember<T>(allocated);

            arenaMembers.push_back(member);
        }

        return ret;
    }
    size_t GetAllocatedSize() { return head_ptr - base_ptr; }

  private:
    size_t size;
    uint8_t *head_ptr;
    uint8_t *base_ptr;
    std::vector<IArenaMember *> arenaMembers;
};
