module decision_tree_io
    use decision_tree_types
    implicit none
contains

!> @brief CSVファイルを読み込む
!> @param[in] filename ファイル名
!> @param[out] dataset データセット
    subroutine readCSV(filename, dataset)
        implicit none
        character(len=*), intent(in) :: filename
        type(data_set), intent(out) :: dataset
        integer :: i, j, ios, ncols, nrows, start, endpos   ! I/Oステータス(ios), 開始位置(start), 終了位置(endpos)
        character(len=8000) :: line                         ! 1行分のデータを格納する変数
        integer :: unit = 10                                ! ファイルユニット
        integer, allocatable :: data(:,:)                   ! 一時的にデータを保存する

        ! 行数と列数の設定
        nrows = NUM_SAMPLES
        ncols = NUM_FEATURES + 1        ! ラベルを含める
        allocate(data(nrows, ncols))    ! データを格納する配列を動的に確保

        ! ファイルを開く
        open(unit=unit, file=filename, status='old', action='read', iostat=ios)
        ! CSVファイルの読み込み
        do i = 1, nrows ! データの行数分繰り返す
            read(unit, '(A)', iostat=ios) line  ! 1行読み込む
            start = 1   ! 開始位置を初期化
            do j = 1, ncols ! データの列数分繰り返す
                endpos = index(line(start:), ',')   ! カンマの位置を取得
                if (endpos == 0) then   ! カンマが見つからなかった場合
                    endpos = len_trim(line(start:)) + 1  ! 行の最後までを読み取る
                end if
                read(line(start:start+endpos-2), '(I3)', iostat=ios) data(i, j)    ! データを読み取る
                start = start + endpos  ! 開始位置を更新
            end do
            ! 最後の要素をラベルとして読み取る
        end do
        ! ファイルを閉じる
        close(unit)

        ! データセットに保存
        allocate(dataset%data(nrows, ncols-1))
        dataset%data = data(:, 1:ncols-1)
        dataset%labels = data(:, ncols)
        dataset%num_samples = nrows
    end subroutine readCSV

!> @brief データセットを学習用とテスト用に分割する
!> @param[in] dataset データセット
!> @param[out] train_data 学習用データセット
!> @param[out] test_data テスト用データセット
!> @param[in] train_ratio 学習用データセットの割合
    subroutine trainTestSplit(dataset, train_data, test_data, train_ratio)
        type(data_set), intent(in) :: dataset
        type(data_set), intent(out) :: train_data, test_data
        real, intent(in) :: train_ratio
        integer :: train_samples, test_samples

        ! それぞれに属するデータの数を計算
        train_samples = int(dataset%num_samples * train_ratio)
        test_samples = dataset%num_samples - train_samples

        ! 学習用データを構造体にコピー
        allocate(train_data%data(train_samples, NUM_FEATURES))
        allocate(train_data%labels(train_samples))
        train_data%num_samples = train_samples
        train_data%data = dataset%data(1:train_samples, :)
        train_data%labels = dataset%labels(1:train_samples)

        ! テスト用データを構造体にコピー
        allocate(test_data%data(test_samples, NUM_FEATURES))
        allocate(test_data%labels(test_samples))
        test_data%num_samples = test_samples
        test_data%data = dataset%data(train_samples+1:dataset%num_samples, :)
        test_data%labels = dataset%labels(train_samples+1:dataset%num_samples)
    end subroutine trainTestSplit

end module decision_tree_io
