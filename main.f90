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
    integer, allocatable :: dataset(:,:)
    integer, allocatable :: labels(:)
    integer :: dataset_size
    integer, allocatable :: train_dataset(:,:)
    integer, allocatable :: train_labels(:)
    integer :: train_samples
    integer, allocatable :: test_dataset(:,:)
    integer, allocatable :: test_labels(:)
    integer :: test_samples
    type(TreeNode), pointer :: tree                     ! 決定木のノードを格納する構造体を宣言
    real :: accuracy, start, finish , elapsed_time      ! 正解率を格納する変数  (realは浮動小数点数を表す)
    integer :: correct_predictions, i, prediction       ! 正解した予測の数を格納する変数, ループ用変数, 予測結果を格納する変数  (integerは整数を表す)
! メインプログラムの処理
    ! データセットの読み込み
    print *, "csvの読み込み中..."
    ! call readCSV("mnist.csv", dataset)
    call readCSV("mnist.csv", dataset, labels, dataset_size)  ! callはサブルーチンを呼び出すときに使う datasetにcsvのデータが格納される
    print *, "csvの読み込み完了!"

    ! trainデータとtestデータの分割
    print *, "データの分割中..."
    call trainTestSplit(dataset, labels, dataset_size, &
            train_dataset, train_labels, train_samples, &
            test_dataset, test_labels, test_samples, 0.7)
    !call trainTestSplit(dataset, train_data, test_data, 0.7)
    print *, "データの分割完了!"
    call cpu_time(start)
    ! 決定木の生成
    print *, "木の生成中..."
    tree => buildTree(train_dataset, train_labels,train_samples, 0)    !buildTreeは関数
    print *, "木の生成完了!"
    correct_predictions = 0

    do i = 1, train_samples                         ! 1からテストデータのサンプル数までループ (%は構造体のメンバにアクセスする演算子)
        prediction = predict(tree, train_dataset(i, :))    ! predictは決定木を用いた予測を行う関数
        if (prediction == train_labels(i)) then
            correct_predictions = correct_predictions + 1   ! 予測が正解していたら正解数を増やす
        end if
    end do

    accuracy = real(correct_predictions) / train_samples    ! 正解率を計算
    print *, "Train Accuracy: ", accuracy * 100, "%"

    correct_predictions = 0
    do i = 1, test_samples                         ! 1からテストデータのサンプル数までループ (%は構造体のメンバにアクセスする演算子)
        prediction = predict(tree, test_dataset(i, :))    ! predictは決定木を用いた予測を行う関数
        if (prediction == test_labels(i)) then
            correct_predictions = correct_predictions + 1   ! 予測が正解していたら正解数を増やす
        end if
    end do

    accuracy = real(correct_predictions) / test_samples    ! 正解率を計算
    print *, "test Accuracy: ", accuracy * 100, "%"

    ! メモリの解放
    call freeTree(tree)
    deallocate(dataset)
    deallocate(labels)
    deallocate(train_dataset)
    deallocate(train_labels)
    deallocate(test_dataset)
    deallocate(test_labels)
    ! プログラムの実行時間を表示
    call cpu_time(finish)
    elapsed_time = finish - start
    print *, '実行時間: ', elapsed_time, ' s'
end program main
