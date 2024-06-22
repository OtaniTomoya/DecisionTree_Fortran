module decision_tree_metrics
    use decision_tree_types
    use decision_tree_split, only: splitDataset  ! splitDatasetサブルーチンの明示的なインポート
    implicit none
contains

    real function giniImpurity(dataset)
        type(data_set), intent(in) :: dataset
        integer :: count(MAX_LABELS)
        integer :: i
        real :: p

        count = 0
        do i = 1, dataset%num_samples
            count(dataset%labels(i) + 1) = count(dataset%labels(i) + 1) + 1
        end do

        giniImpurity = 1.0
        do i = 1, MAX_LABELS
            if (dataset%num_samples > 0) then
                p = real(count(i)) / dataset%num_samples
                giniImpurity = giniImpurity - p * p
            end if
        end do
    end function giniImpurity

    real function informationGain(dataset, feature, threshold)
        type(data_set), intent(in) :: dataset
        integer, intent(in) :: feature
        integer, intent(in) :: threshold
        type(data_set) :: left, right
        real :: parent_impurity, left_impurity, right_impurity
        real :: left_weight, right_weight

        call splitDataset(dataset, feature, threshold, left, right)

        parent_impurity = giniImpurity(dataset)
        left_impurity = giniImpurity(left)
        right_impurity = giniImpurity(right)

        left_weight = real(left%num_samples) / dataset%num_samples
        right_weight = real(right%num_samples) / dataset%num_samples

        informationGain = parent_impurity - left_weight * left_impurity - right_weight * right_impurity

        deallocate(left%data)
        deallocate(left%labels)
        deallocate(right%data)
        deallocate(right%labels)
    end function informationGain

end module decision_tree_metrics
