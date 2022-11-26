BEGIN { 
    print "hello" "," "world" 
}

{
    print 
}

END {
    print FNR, NR
}
