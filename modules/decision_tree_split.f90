module decision_tree_split
    use decision_tree_types
    implicit none
contains

    subroutine splitDataset(dataset, feature, threshold, left, right)
        type(data_set), intent(in) :: dataset
        integer, intent(in) :: feature
        integer, intent(in) :: threshold
        type(data_set), intent(out) :: left, right
        integer :: left_count, right_count
        integer :: i

        left_count = count(dataset%data(:, feature) < threshold)
        right_count = dataset%num_samples - left_count

        allocate(left%data(left_count, dataset%num_features))
        allocate(left%labels(left_count))
        left%num_samples = left_count
        left%num_features = dataset%num_features

        allocate(right%data(right_count, dataset%num_features))
        allocate(right%labels(right_count))
        right%num_samples = right_count
        right%num_features = dataset%num_features

        left_count = 0
        right_count = 0

        do i = 1, dataset%num_samples
            if (dataset%data(i, feature) < threshold) then
                left_count = left_count + 1
                left%data(left_count, :) = dataset%data(i, :)
                left%labels(left_count) = dataset%labels(i)
            else
                right_count = right_count + 1
                right%data(right_count, :) = dataset%data(i, :)
                right%labels(right_count) = dataset%labels(i)
            end if
        end do
    end subroutine splitDataset

end module decision_tree_split
