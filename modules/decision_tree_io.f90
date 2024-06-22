module decision_tree_io
    use decision_tree_types
    implicit none
contains

    subroutine readCSV(filename, dataset)
        implicit none
        character(len=*), intent(in) :: filename
        type(data_set), intent(out) :: dataset
        integer :: i, j, ios, ncols, nrows, start, endpos
        character(len=8000) :: line
        integer :: unit = 10
        integer, allocatable :: data(:,:)
        integer, allocatable :: labels(:)

        ! 行数と列数の設定
        nrows = NUM_SAMPLES
        ncols = NUM_FEATURES + 1
        allocate(data(nrows, ncols))
        allocate(labels(nrows))

        ! ファイルを開く
        open(unit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
            print *, 'Error opening file: ', filename
            stop
        end if

        ! CSVファイルの読み込み
        do i = 1, nrows
            read(unit, '(A)', iostat=ios) line
            start = 1
            do j = 1, ncols
                ! カンマで区切られたデータを抽出
                endpos = index(line(start:), ',')
                if (endpos == 0) then
                    endpos = len_trim(line(start:)) + 1
                end if
                read(line(start:start+endpos-2), '(I10)', iostat=ios) data(i, j)
                start = start + endpos
            end do
            ! 最後の要素をラベルとして読み取る
            labels(i) = data(i, ncols)  ! ラベルを整数に変換
        end do
        ! ファイルを閉じる
        close(unit)

        ! データセットに保存
        allocate(dataset%data(nrows, ncols-1))
        allocate(dataset%labels(nrows))
        dataset%data = data(:, 1:ncols-1)
        dataset%labels = labels
        dataset%num_samples = nrows
        dataset%num_features = ncols - 1
    end subroutine readCSV

    subroutine trainTestSplit(dataset, train_data, test_data, train_ratio)
        type(data_set), intent(in) :: dataset
        type(data_set), intent(out) :: train_data, test_data
        real, intent(in) :: train_ratio
        integer :: train_samples, test_samples

        train_samples = int(dataset%num_samples * train_ratio)
        test_samples = dataset%num_samples - train_samples

        allocate(train_data%data(train_samples, dataset%num_features))
        allocate(train_data%labels(train_samples))
        train_data%num_samples = train_samples
        train_data%num_features = dataset%num_features

        allocate(test_data%data(test_samples, dataset%num_features))
        allocate(test_data%labels(test_samples))
        test_data%num_samples = test_samples
        test_data%num_features = dataset%num_features

        train_data%data = dataset%data(1:train_samples, :)
        train_data%labels = dataset%labels(1:train_samples)

        test_data%data = dataset%data(train_samples+1:dataset%num_samples, :)
        test_data%labels = dataset%labels(train_samples+1:dataset%num_samples)
    end subroutine trainTestSplit

end module decision_tree_io
