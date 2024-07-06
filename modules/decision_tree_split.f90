module decision_tree_split
    use decision_tree_types
    implicit none
contains
    subroutine splitDataset(dataset, labels, num_samples, feature, threshold, left_dataset, left_labels, left_samples, &
            right_dataset, right_labels, right_samples)
        integer, allocatable, intent(in) :: dataset(:,:)
        integer, allocatable, intent(in) :: labels(:)
        integer, intent(in) :: num_samples
        integer, intent(in) :: feature
        integer, intent(in) :: threshold
        integer, allocatable, intent(out) :: left_dataset(:,:)
        integer, allocatable, intent(out) :: left_labels(:)
        integer, intent(out) :: left_samples
        integer, allocatable, intent(out) :: right_dataset(:,:)
        integer, allocatable, intent(out) :: right_labels(:)
        integer, intent(out) :: right_samples
        integer :: left_count, right_count
        integer :: i

        ! データセットの分割
        left_count = count(dataset(:, feature) < threshold)
        right_count = num_samples - left_count

        ! 右側のデータセットのメモリ確保
        allocate(left_dataset(left_count, NUM_FEATURES))
        allocate(left_labels(left_count))
        left_samples = left_count

        ! 右側のデータセットのメモリ確保
        allocate(right_dataset(right_count, NUM_FEATURES))
        allocate(right_labels(right_count))
        right_samples = right_count
        !カウント
        left_count = 0
        right_count = 0

        do i = 1, num_samples
            if (dataset(i, feature) < threshold) then
                left_count = left_count + 1
                left_dataset(left_count, :) = dataset(i, :)
                left_labels(left_count) = labels(i)
            else
                right_count = right_count + 1
                right_dataset(right_count, :) = dataset(i, :)
                right_labels(right_count) = labels(i)
            end if
        end do
    end subroutine splitDataset

end module decision_tree_split
