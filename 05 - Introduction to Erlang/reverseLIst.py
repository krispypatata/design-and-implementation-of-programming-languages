# tail recursion - reversing a list

def reverseListTR(numList, reversedList):
    if (len(numList) <= 0):
        return reversedList

    left = numList[:-1]
    right = reversedList + [numList[-1]]
    return reverseListTR(left, right)

def reverseList(numList):
    return reverseListTR(numList, [])

list1 = [3, 6, 9, 1, 2]

print(list1)
print(reverseList(list1))
