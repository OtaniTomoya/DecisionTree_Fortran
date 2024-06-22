module decision_tree_types
    implicit none

    integer, parameter :: NUM_FEATURES = 784
    integer, parameter :: NUM_SAMPLES = 70
    integer, parameter :: MAX_LABELS = 10

    type :: TreeNode
        integer :: feature
        integer :: threshold
        integer :: label
        type(TreeNode), pointer :: left => null(), right => null()
    end type TreeNode

    type :: data_set
        integer, allocatable :: data(:,:)
        integer, allocatable :: labels(:)
        integer :: num_samples
        integer :: num_features
    end type data_set

end module decision_tree_types
