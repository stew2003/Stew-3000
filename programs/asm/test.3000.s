entry:
    mvi 0, a
    mvi 1, b
    cmp a, b
    jae stop
    outi -1
    hlt
stop:
    outi 1
    hlt
