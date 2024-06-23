module decision_tree_build
    use decision_tree_types
    use decision_tree_utils
    use decision_tree_split
    use decision_tree_metrics
    implicit none
contains
    ! recursive:再帰的な関数を定義
    !> 決定木を構築する再帰関数
    ! @param[in] dataset データセット
    ! @param[in] depth 深さ
    ! @return 決定木のノードを表す構造体
    recursive function buildTree(dataset, depth) result(node)
    ! 変数を宣言
        type(data_set), intent(in) :: dataset           ! データセット
        integer, intent(in) :: depth                    ! 深さ
        type(TreeNode), pointer :: node                 ! ノード
        integer :: i, j, best_feature, best_threshold, threshold   ! ループ用の変数(i, j), 最良の特徴・閾値
        real :: best_gain, gain                         ! 最良の情報利得, 情報利得
        type(data_set) :: left, right                   ! 左のデータセット, 右のデータセット

        if (dataset%num_samples == 0) then  ! データセットのサンプル数が0の場合
            node => null()                  ! ノードをnullにする    ノードの初期値がnullなのでいらない．後で直す
            return
        end if

        best_gain = 0.0 ! 最良の情報利得を初期化

        ! 最良の特徴量・閾値を探す
            ! 特徴の列に存在する全ての値について閾値と仮定して情報利得を計算し，最良の情報利得を持つ特徴と閾値を見つけるのが本来の動作であるが，
            ! 特徴量は0~255の間の自然数であるためこのようにしている
        do i = 1, NUM_FEATURES  ! 特徴の数だけループ
            do j = 0, 25               ! 0から255までループ
                threshold = j * 10   ! 閾値を設定
                gain = informationGain(dataset, i, threshold)   ! 情報利得を計算 informationGainは(data, feature, threshold)を引数に取る関数
                ! 最適な特徴と閾値を更新
                if (gain > best_gain) then
                    best_gain = gain
                    best_feature = i
                    best_threshold = threshold
                end if
            end do
        end do

        ! 最良の情報利得が0の場合，葉ノードとする
        if (best_gain == 0.0) then
            node => createNode(-1, 0, dataset%labels(1))
            return
        end if

        ! 最適な特徴と閾値でデータセットを分割
        call splitDataset(dataset, best_feature, best_threshold, left, right)

        ! このノードから見て左右のノードを再帰的に構築
        node => createNode(best_feature, best_threshold, -1)
        node%left => buildTree(left, depth + 1)
        node%right => buildTree(right, depth + 1)
        print *, "Depth: ", depth, "Feature: ", best_feature, "Threshold: ", best_threshold, "Gain: ", best_gain
        ! データセットを解放
            ! この処理が実行される時は，葉ノードまで到達した時であることに注意
        deallocate(left%data)
        deallocate(left%labels)
        deallocate(right%data)
        deallocate(right%labels)
    end function buildTree

    !> 決定木を使ってサンプルを予測する再帰関数
    ! @param[in] tree 決定木のノード
    ! @param[in] sample サンプル
    ! @return 予測されたラベル
    recursive function predict(tree, sample) result(label)
        ! 変数を宣言
        type(TreeNode), pointer :: tree     ! 決定木のノード
        integer, intent(in) :: sample(:)    ! サンプル
        integer :: label                    ! 予測されたラベル

        ! ノードが葉ノードの場合，ラベルを返す
        if (tree%feature == -1) then
            label = tree%label
            return
        end if

        ! 特徴と閾値に基づいて左右のノードを選択
        if (sample(tree%feature) < tree%threshold) then
            label = predict(tree%left, sample)  ! 左のノードを選択
        else
            label = predict(tree%right, sample) ! 右のノードを選択
        end if
    end function predict

end module decision_tree_build
