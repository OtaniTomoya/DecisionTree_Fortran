program main
    use decision_tree_types
    use decision_tree_utils
    use decision_tree_io
    use decision_tree_split
    use decision_tree_metrics
    use decision_tree_build
    implicit none

    type(data_set) :: dataset, train_data, test_data
    type(TreeNode), pointer :: tree
    real :: accuracy
    integer :: correct_predictions, i, prediction

    print *, "csvの読み込み中..."
    call readCSV("mnist.csv", dataset)
    print *, "csvの読み込み完了!"

    print *, "データの分割中..."
    call trainTestSplit(dataset, train_data, test_data, 0.7)
    print *, "データの分割完了!"
    print *, "木の生成中..."
    tree => buildTree(train_data, 0)
    print *, "木の生成完了!"
    correct_predictions = 0

    do i = 1, test_data%num_samples
        prediction = predict(tree, test_data%data(i, :))
        if (prediction == test_data%labels(i)) correct_predictions = correct_predictions + 1
        !print *, prediction, ",", test_data%labels(i)
    end do

    accuracy = real(correct_predictions) / test_data%num_samples
    print *, "Accuracy: ", accuracy * 100, "%"

    call freeTree(tree)
    deallocate(dataset%data)
    deallocate(dataset%labels)
    deallocate(train_data%data)
    deallocate(train_data%labels)
    deallocate(test_data%data)
    deallocate(test_data%labels)
end program main
