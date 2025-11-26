doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y   
main = 28 `doubleUs` 88 + doubleMe 123  