#include "utils/memory_arean.h"
#include "ztest.h"

TEST(MemoryArenaAllocationTest)
{
    MemoryArena arena(8);
    int *i = arena.Allocate<int>(1);
    int *i2 = arena.Allocate<int>(1);

    ASSERT_EQ(arena.GetAllocatedSize(), 8, "Allocated size should be 8");
}

TEST(MemoryArenaArrayAllocationTest)
{
    MemoryArena arena(6 * sizeof(float));

    float *fArr = arena.AllocateArray<float>(6, 5.0);
    size_t allocatedSize = arena.GetAllocatedSize();
    ASSERT_EQ(allocatedSize, 6 * sizeof(float), "Allocated size should be 24");

    for (size_t i = 0; i < 6; i++)
    {
        ASSERT_EQ(fArr[i], 5.0, "Value should be 5.0");
    }
}
