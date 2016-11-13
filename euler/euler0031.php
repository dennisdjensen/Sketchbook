<?php
# Copyright 2016 Dennis Decker Jensen
# Date: 13 November 2016
# Purpose: Count coin change
# Tectonics: php euler0031.php

define('SUBSEP', ':');

$usd_coins_all = array(100, 50, 25, 10, 5, 1);
$usd_coins = array_slice($usd_coins_all, 2);
$eur_coins = array(200, 100, 50, 20, 10, 5, 2, 1);
$gbp_coins = $eur_coins;
$cdn_coins_all = array(200, 100, 50, 25, 10, 5, 1);
$cdn_coins     = array(200, 100,     25, 10, 5, 1);

$all_coins = array($usd_coins_all, $usd_coins, $eur_coins,
		$gbp_coins, $cdn_coins_all, $cdn_coins);
$names = array("ALL USD", "USD", "EUR", "GBP", "ALL CDN", "CDN");

// Too slow
function count1($amount, $coins) {
	if ($amount < 0) return 0;
	if (count($coins) == 0) return 0;
	if ($amount == 0) return 1;
	return count1($amount - $coins[0], $coins) +
		count1($amount, array_slice($coins, 1));
}

// Memoized count1: cache and with bcmath for large numbers
// Too slow
function count2($amount, $coins, $length, $cache = array()) {
	if ($amount < 0) return 0;
	if (count($coins) == 0) return 0;
	if ($amount == 0) return 1;
	$key = $amount . SUBSEP . $length;
	if (isset($cache[$key])) return $cache[$key];
	return $cache[$key] = bcadd(
		count2(bcsub($amount, $coins[0]), $coins,
				$length, $cache),
		count2($amount, array_slice($coins, 1),
				$length - 1, $cache));
}

// Iterative count change
// Memory overflow in inner loop at array assignment
function count3($amount, $coins) {
	$ways = array_fill(0, $amount + 1, 0);
	$ways[0] = 1;
	foreach ($coins as $coin)
		for ($j = $coin; $j <= $amount; ++$j)
			$ways[$j] = bcadd(
				$ways[$j], $ways[bcsub($j, $coin)]);
	return $ways[$amount];
}

printf("%8d %7s: %d\n",  200, $names[3],
		count3(200,  $all_coins[3])); // GBP

printf("%8d %7s: %d\n",  100, $names[0],
		count3(100,  $all_coins[0])); // USD
printf("%8d %7s: %d\n",  100, $names[1],
		count3(100,  $all_coins[1]));
printf("%8d %7s: %d\n", 100000, $names[0],
		count3(1000, $all_coins[0]));
print("\n");

// bcmath is needed for these numbers:
for ($i = 0; $i < count($names); ++$i) {
	printf("%8d %7s: %d\n", 100, $names[$i],
		count3(100, $all_coins[$i]));
	printf("%8d %7s: %d\n", 1000, $names[$i],
		count3(1000, $all_coins[$i]));
	for ($j = 100000; $j <= 10000000; $j *= 10)
		printf("%8d %7s: %d\n", $j, $names[$i],
			count3($j, $all_coins[$i]));
}
?>
