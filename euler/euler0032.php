<?php
# Copyright 2016 Dennis Decker Jensen
# Date: 14 November 2016
# Purpose: Sum of pandigital products
# Tectonics: php euler0032.php

// Taken from the PHP Cookbook (O'Reilly)
//   by David Sklar and Adam Trachtenberg,
// in turn taken from an idea by Mark-Jason Dominus,
// in turn taken from the Perl Cookbook (O'Reilly)
//   by Tom Christiansen and Nathan Torkington,
// and
// A Discipline of Programming (Prentice-Hall)
//   by Edsger Dijkstra.
function pc_next_permutation($p, $last) {
	// Find last element in order: $p[$i] < $p[$i+1]
	for ($i = $last - 1; $i <> -1 && $p[$i] >= $p[$i+1]; --$i)
		;

	// At the end, we end up with the array reversed.
	// Example: (1, 2, 3, 4) => (4, 3, 2, 1).
	if ($i == -1) return false;

	// Find last element with number bigger than $p[$i]
	for ($j = $last; $p[$j] <= $p[$i]; --$j)
		;

	// Swap, so the bigger number moves toward the start,
	// and vice versa.
	$hold = $p[$i]; $p[$i] = $p[$j]; $p[$j] = $hold;

	// Reverse the elements in between $i and $last
	for (++$i, $j = $last; $i < $j; ++$i, --$j) {
		$hold = $p[$i]; $p[$i] = $p[$j]; $p[$j] = $hold;
	}

	return $p;
}

$digits = range(1,9);
$sum = 0;
$seen = array();
do {
	// Case X * YYYY = ZZZZ
	$a = $digits[0];
	$b = (int)implode("", array_slice($digits, 1, 4));
	$c = (int)implode("", array_slice($digits, 5, 4));
	$product = $a * $b;
	if ($product == $c && !isset($seen[$product])) {
		$sum += $product;
		$seen[$product] = TRUE;
		print("$a * $b = $product\n");
	}
	// Case XX * YYY = ZZZZ
	$a = (int)implode("", array_slice($digits, 0, 2));
	$b = (int)implode("", array_slice($digits, 2, 3));
	$c = (int)implode("", array_slice($digits, 5, 4));
	$product = $a * $b;
	if ($product == $c && !isset($seen[$product])) {
		$sum += $product;
		$seen[$product] = TRUE;
		print("$a * $b = $product\n");
	}
	// Remaining cases are repeats of the above cases.
} while ($digits = pc_next_permutation($digits, 8));

echo "Pandigital product sum: $sum\n";
?>
