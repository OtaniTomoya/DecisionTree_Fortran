program main
    ! モジュールの使用宣言
    use decision_tree_types
    use decision_tree_utils
    use decision_tree_io
    use decision_tree_split
    use decision_tree_metrics
    use decision_tree_build

    ! 変数が明示的に宣言されていることを示す(高速化とバグの防止に役立つ)
    implicit none

    ! このprogramで使用する変数の宣言
        ! type(name)で，自分で決めたデータ型が使える(今回は構造体のように使っている)

    type(data_set) :: dataset, train_data, test_data    ! データセットの構造体を宣言
    type(TreeNode), pointer :: tree                     ! 決定木のノードを格納する構造体を宣言
    real :: accuracy, start, finish , elapsed_time      ! 正解率を格納する変数  (realは浮動小数点数を表す)
    integer :: correct_predictions, i, prediction       ! 正解した予測の数を格納する変数, ループ用変数, 予測結果を格納する変数  (integerは整数を表す)
! メインプログラムの処理
    ! データセットの読み込み
    print *, "csvの読み込み中..."
    call readCSV("mnist.csv", dataset)  ! callはサブルーチンを呼び出すときに使う datasetにcsvのデータが格納される
    print *, "csvの読み込み完了!"

    ! trainデータとtestデータの分割
    print *, "データの分割中..."
    call trainTestSplit(dataset, train_data, test_data, 0.7)
    print *, "データの分割完了!"
    call cpu_time(start)
    ! 決定木の生成
    print *, "木の生成中..."
    tree => buildTree(train_data, 0)    !buildTreeは関数
    print *, "木の生成完了!"
    correct_predictions = 0

    do i = 1, train_data%num_samples                         ! 1からテストデータのサンプル数までループ (%は構造体のメンバにアクセスする演算子)
        prediction = predict(tree, train_data%data(i, :))    ! predictは決定木を用いた予測を行う関数
        if (prediction == train_data%labels(i)) then
            correct_predictions = correct_predictions + 1   ! 予測が正解していたら正解数を増やす
        end if
    end do

    accuracy = real(correct_predictions) / train_data%num_samples    ! 正解率を計算
    print *, "Train Accuracy: ", accuracy * 100, "%"

    correct_predictions = 0
    do i = 1, test_data%num_samples                         ! 1からテストデータのサンプル数までループ (%は構造体のメンバにアクセスする演算子)
        prediction = predict(tree, test_data%data(i, :))    ! predictは決定木を用いた予測を行う関数
        if (prediction == test_data%labels(i)) then
            correct_predictions = correct_predictions + 1   ! 予測が正解していたら正解数を増やす
        end if
    end do

    accuracy = real(correct_predictions) / test_data%num_samples    ! 正解率を計算
    print *, "test Accuracy: ", accuracy * 100, "%"

    ! メモリの解放
    call freeTree(tree)
    deallocate(dataset%data)
    deallocate(dataset%labels)
    deallocate(train_data%data)
    deallocate(train_data%labels)
    deallocate(test_data%data)
    deallocate(test_data%labels)
    ! プログラムの実行時間を表示
    call cpu_time(finish)
    elapsed_time = finish - start
    print *, '実行時間: ', elapsed_time, ' s'
end program main
