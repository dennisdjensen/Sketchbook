<?php
# Copyright 2016 Dennis Decker Jensen
# Date: 13 November 2016
# Purpose: Distinct powers from a**b, 2 <= a <= 100, 2 <= b <= 100
# Tectonics: php euler0029.php

function distinct_powers($amin, $amax, $bmin, $bmax) {
	$ds = array();
	for ($a = $amin; $a <= $amax; ++$a)
		for ($b = $bmin; $b <= $bmax; ++$b)
			$ds{bcpow($a, $b)} = TRUE;
	return array_keys($ds);
}

$ds = distinct_powers(2, 5, 2, 5);
sort($ds);
echo count($ds) . " distinct powers:\n";
echo "  " . implode(', ', $ds) . "\n\n";

$n = count(distinct_powers(2, 100, 2, 100));
echo "For 2 <= a <= 100, 2 <= b <= 100, there are $n distinct powers a**b\n";
?>
