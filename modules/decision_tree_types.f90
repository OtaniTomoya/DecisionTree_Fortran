module decision_tree_types
    implicit none

    integer, parameter :: NUM_FEATURES = 784
    integer, parameter :: DATASET_SIZE = 70000
    integer, parameter :: MAX_LABELS = 9

    type :: TreeNode
        integer :: feature
        integer :: threshold
        integer :: label
        type(TreeNode), pointer :: left => null(), right => null()
    end type TreeNode

end module decision_tree_types
