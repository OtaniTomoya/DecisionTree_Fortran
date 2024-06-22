module decision_tree_metrics
    use decision_tree_types
    use decision_tree_split, only: splitDataset  ! splitDatasetサブルーチンの明示的なインポート
    implicit none
contains
    !> @brief ジニ不純度を計算する関数
    !! @param[in] dataset データセット
    !! @return ジニ不純度
    real function giniImpurity(dataset)
        type(data_set), intent(in) :: dataset
        integer :: count(0:MAX_LABELS)
        integer :: i
        real :: p

        ! ラベルの出現回数をカウント
        count = 0
        do i = 1, dataset%num_samples
            count(dataset%labels(i)) = count(dataset%labels(i)) + 1
        end do

        ! ジニ不純度を計算
        giniImpurity = 1.0
        do i = 0, MAX_LABELS
            if (dataset%num_samples > 0 .and. count(i) > 0) then
                p = real(count(i)) / dataset%num_samples
                giniImpurity = giniImpurity - p * p
            end if
        end do
    end function giniImpurity

    !> @brief 情報利得を計算する関数
    !! @param[in] dataset データセット
    !! @param[in] feature 特徴量のインデックス
    !! @param[in] threshold 閾値
    !! @return 情報利得
    real function informationGain(dataset, feature, threshold)
        type(data_set), intent(in) :: dataset
        integer, intent(in) :: feature
        integer, intent(in) :: threshold
        type(data_set) :: left, right
        real :: parent_impurity, left_impurity, right_impurity  ! ジニ不純度
        real :: left_weight, right_weight                       ! 左右のデータセットの重み

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
