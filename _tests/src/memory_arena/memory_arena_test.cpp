#include "utils/memory_arean.h"
#include "ztest.h"

TEST(MemoryArenaAllocationTest)
{
    MemoryArena arena(8);
    int *i = arena.Allocate<int>(1);
    int *i2 = arena.Allocate<int>(1);

    ASSERT_EQ(arena.GetAllocatedSize(), 8, "Allocated size should be 8");
}
