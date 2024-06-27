module decision_tree_metrics
    use decision_tree_types
    use decision_tree_split, only: splitDataset
    implicit none
contains
    real function giniImpurity(labels, num_samples)
        integer, intent(in) :: num_samples
        integer, intent(in) :: labels(num_samples)
        integer :: label_count(0:MAX_LABELS)
        integer :: i
        real :: p

        ! ラベルの出現回数をカウント
        label_count = 0
        do i = 1, num_samples
            label_count(labels(i)) = label_count(labels(i)) + 1
        end do

        ! ジニ不純度を計算
        giniImpurity = 1.0
        do i = 0, MAX_LABELS
            if (num_samples > 0 .and. label_count(i) > 0) then
                p = real(label_count(i)) / num_samples
                giniImpurity = giniImpurity - p * p
            end if
        end do
    end function giniImpurity

    real function informationGain(dataset, feature, threshold, parent_impurity)
        type(data_set), intent(in) :: dataset
        integer, intent(in) :: feature
        integer, intent(in) :: threshold
        real, intent(in) :: parent_impurity
        integer :: left(dataset%num_samples), right(dataset%num_samples)
        integer :: left_count, right_count
        real :: left_impurity, right_impurity
        real :: left_weight, right_weight

        call splitlabels(dataset, feature, threshold, left, left_count, right, right_count)

        left_impurity = giniImpurity(left, left_count)
        right_impurity = giniImpurity(right, right_count)

        left_weight = real(left_count) / dataset%num_samples
        right_weight = real(right_count) / dataset%num_samples

        informationGain = parent_impurity - left_weight * left_impurity - right_weight * right_impurity
    end function informationGain

    subroutine splitlabels(dataset, feature, threshold, left,left_count, right, right_count)
        type(data_set), intent(in) :: dataset
        integer, intent(in) :: feature
        integer, intent(in) :: threshold
        integer, intent(out) :: left_count, right_count
        integer, intent(out) :: left(dataset%num_samples), right(dataset%num_samples)
        integer :: i

        left_count = 0
        right_count = 0

        do i = 1, dataset%num_samples
            if (dataset%data(i, feature) < threshold) then
                left_count = left_count + 1
                left(left_count) = dataset%labels(i)
            else
                right_count = right_count + 1
                right(right_count) = dataset%labels(i)
            end if
        end do
    end subroutine splitlabels
end module decision_tree_metrics
