module decision_tree_utils
    use decision_tree_types
    implicit none
contains

    function createNode(feature, threshold, label) result(node)
        integer, intent(in) :: feature
        integer, intent(in) :: threshold
        integer, intent(in) :: label
        type(TreeNode), pointer :: node

        allocate(node)
        node%feature = feature
        node%threshold = threshold
        node%label = label
        node%left => null()
        node%right => null()
    end function createNode

    recursive subroutine freeTree(node)
        type(TreeNode), pointer :: node
        if (.not. associated(node)) return
        call freeTree(node%left)
        call freeTree(node%right)
        nullify(node%left)
        nullify(node%right)
        deallocate(node)
    end subroutine freeTree

end module decision_tree_utils
