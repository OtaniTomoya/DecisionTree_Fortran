module decision_tree_build
    use decision_tree_types
    use decision_tree_utils
    use decision_tree_split
    use decision_tree_metrics
    implicit none
contains
    recursive function buildTree(dataset, labels, num_samples, depth) result(node)
        integer, allocatable, intent(in) :: dataset(:,:)
        integer, allocatable, intent(in) :: labels(:)
        integer, intent(in) :: num_samples
        integer, intent(in) :: depth
        type(TreeNode), pointer :: node
        integer :: i, j, best_feature, best_threshold, threshold
        real :: best_gain, gain, parent_impurity
        integer, allocatable :: left_dataset(:,:)
        integer, allocatable :: left_labels(:)
        integer :: left_samples
        integer, allocatable :: right_dataset(:,:)
        integer, allocatable :: right_labels(:)
        integer :: right_samples
        integer, dimension(256) :: unique_labels
        if (num_samples == 0) then
            node => null()
            return
        end if
        unique_labels = -1
        best_gain = 0.0

        ! 最良の特徴量・閾値を探す
        call get_unique_labels(labels, unique_labels)
        parent_impurity = giniImpurity(labels, num_samples)
        do i = 1, NUM_FEATURES
            do j = 1, 255
                if (unique_labels(j) == -1) then
                    exit
                end if
                threshold = unique_labels(j)
                gain = informationGain(dataset, labels, num_samples, i, threshold, parent_impurity)
                if (gain > best_gain) then
                    best_gain = gain
                    best_feature = i
                    best_threshold = threshold
                end if
            end do
        end do

        ! 最良の情報利得が0の場合，葉ノードとする
        if (best_gain == 0.0) then
            node => createNode(-1, 0, labels(1))
            return
        end if

        call splitDataset(dataset, labels, num_samples, best_feature, best_threshold, &
                left_dataset, left_labels, left_samples, right_dataset, right_labels, right_samples)

        node => createNode(best_feature, best_threshold, -1)
        node%left => buildTree(left_dataset, left_labels, left_samples, depth + 1)
        node%right => buildTree(right_dataset, right_labels, right_samples, depth + 1)
        deallocate(left_dataset)
        deallocate(left_labels)
        deallocate(right_dataset)
        deallocate(right_labels)
    end function buildTree

    subroutine get_unique_labels(input_labels, output_labels)
        integer, dimension(:), intent(in) :: input_labels
        integer, dimension(256), intent(out) :: output_labels
        integer :: i, j, count
        logical :: is_unique

        count = 1
        output_labels = -1  ! 初期化

        ! 一時的な配列にユニークなラベルを保存
        do i = 1, size(input_labels)
            if (count > 256) then
                exit
            end if
            is_unique = .true.
            do j = 1, count
                if (input_labels(i) == output_labels(j)) then
                    is_unique = .false.
                    exit
                end if
            end do
            if (is_unique) then
                output_labels(count) = input_labels(i)
                count = count + 1
            end if
        end do
    end subroutine get_unique_labels

    recursive function predict(tree, sample) result(label)
        type(TreeNode), pointer :: tree
        integer, intent(in) :: sample(:)
        integer :: label

        ! ノードが葉ノードの場合，ラベルを返す
        if (tree%feature == -1) then
            label = tree%label
            return
        end if

        ! 特徴と閾値に基づいて左右のノードを選択
        if (sample(tree%feature) < tree%threshold) then
            label = predict(tree%left, sample)
        else
            label = predict(tree%right, sample)
        end if
    end function predict

end module decision_tree_build
