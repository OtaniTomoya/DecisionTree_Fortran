module decision_tree_metrics
    use decision_tree_types
    implicit none
contains
    real function giniImpurity(labels, num_samples)
        integer, intent(in) :: num_samples, labels(num_samples)
        integer :: label_count(0:MAX_LABELS), i
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

    real function informationGain(dataset, labels, num_samples, feature, threshold, parent_impurity)
        integer, allocatable, intent(in) :: dataset(:,:)
        integer, allocatable, intent(in) :: labels(:)
        integer, intent(in) :: num_samples, feature, threshold
        real, intent(in) :: parent_impurity
        integer :: left(num_samples), right(num_samples), left_count, right_count
        real :: left_impurity, right_impurity, left_weight, right_weight

        call splitlabels(dataset, labels, num_samples, feature, threshold, left, left_count, right, right_count)

        left_impurity = giniImpurity(left, left_count)
        right_impurity = giniImpurity(right, right_count)

        left_weight = real(left_count) / num_samples
        right_weight = real(right_count) / num_samples

        informationGain = parent_impurity - left_weight * left_impurity - right_weight * right_impurity
    end function informationGain

    subroutine splitlabels(dataset, labels, num_samples, feature, threshold, left,left_count, right, right_count)
        integer, allocatable, intent(in) :: dataset(:,:)
        integer, allocatable, intent(in) :: labels(:)
        integer, intent(in) :: num_samples, feature, threshold
        integer, intent(out) :: left_count, right_count, left(num_samples), right(num_samples)
        integer :: i

        left_count = 0
        right_count = 0

        do i = 1, num_samples
            if (dataset(i, feature) < threshold) then
                left_count = left_count + 1
                left(left_count) = labels(i)
            else
                right_count = right_count + 1
                right(right_count) = labels(i)
            end if
        end do
    end subroutine splitlabels
end module decision_tree_metrics
