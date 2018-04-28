<?php
# Copyright 2016 Dennis Decker Jensen
# Date: 13 November 2016
# Purpose: Digit fifth powers
# Tectonics: php euler030.php

function power_sum($number, $power) {
	$sum = 0;
	foreach (str_split($number, 1) as $digit)
		$sum += $digit ** $power;
	return $sum;
}

function sum_digit_powers($power) {
	printf(" %d => \n", $power);
	$sum = 0;
	$n = 2;
	$maxsum = strlen($n) * 9 ** $power;
	$psum = power_sum($n, $power);
	while (strlen($n) <= strlen($maxsum)) {
		if ($n == $psum) {
			$sum += $psum;
			printf("    %8d\n", $psum);
		}
		++$n;
		$maxsum = strlen($n) * 9 ** $power;
		$psum = power_sum($n, $power);
	}
	printf(" => %8d\n", $sum);
	return $sum;
}
sum_digit_powers(4);
sum_digit_powers(5);
?>
