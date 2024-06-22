module decision_tree_build
    use decision_tree_types
    use decision_tree_utils
    use decision_tree_split
    use decision_tree_metrics
    implicit none
contains

    recursive function buildTree(dataset, depth) result(node)
        type(data_set), intent(in) :: dataset
        integer, intent(in) :: depth
        type(TreeNode), pointer :: node
        integer :: i, j, best_feature, best_threshold
        real :: best_gain, gain
        type(data_set) :: left, right

        if (dataset%num_samples == 0) then
            node => null()
            return
        end if

        best_gain = 0.0

        do i = 1, dataset%num_features
            do j = 0, 255
                gain = informationGain(dataset, i, j)
                if (gain > best_gain) then
                    best_gain = gain
                    best_feature = i
                    best_threshold = j
                end if
            end do
        end do

        if (best_gain == 0.0) then
            node => createNode(-1, 0, dataset%labels(1))
            return
        end if

        call splitDataset(dataset, best_feature, best_threshold, left, right)

        node => createNode(best_feature, best_threshold, -1)
        node%left => buildTree(left, depth + 1)
        node%right => buildTree(right, depth + 1)

        deallocate(left%data)
        deallocate(left%labels)
        deallocate(right%data)
        deallocate(right%labels)
    end function buildTree

    recursive function predict(tree, sample) result(label)
        type(TreeNode), pointer :: tree
        integer, intent(in) :: sample(:)
        integer :: label

        if (.not. associated(tree)) then
            print *, "Error: Tree is not associated"
            stop
        end if

        if (tree%feature == -1) then
            label = tree%label
            return
        end if

        if (sample(tree%feature) < tree%threshold) then
            label = predict(tree%left, sample)
        else
            label = predict(tree%right, sample)
        end if
    end function predict

end module decision_tree_build
