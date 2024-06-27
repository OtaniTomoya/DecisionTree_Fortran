program test_decision_tree_metrics
    use decision_tree_metrics
    use decision_tree_types
    implicit none

    integer :: labels(NUM_SAMPLES)
    integer :: i, threshold
    real :: gini, info_gain
    real :: lab

    ! Generate random labels
    call random_seed()
    do i=1, NUM_SAMPLES
        call random_number(lab)
        labels(i) = int(lab * MAX_LABELS)
    end do

    ! Test giniImpurity function
    gini = giniImpurity(labels, NUM_SAMPLES)
    print *, "Gini Impurity:", gini

    ! Test informationGain function
    threshold = int(lab*NUM_FEATURES)
    info_gain = informationGain(labels, 1, 0, threshold)
    print *, "Information Gain:", info_gain

end program test_decision_tree_metrics
