# Largest ordered word
# Copyright 2015 Dennis Decker Jensen
# Tectonics: awk -f ordered-words.awk < /usr/share/dict/words

BEGIN {
        maxlen = length(max = "")
}
{
        word = $0
        len = length(word)
        if (len < maxlen) next
        for (i = 1; i <= len - 1; ++i)
                if (substr(word, i, 1) < substr(word, i + 1, 1))
                        maxlen = length(max = word)
}
END {
        printf("Largest word '%s' is %d characters long\n", max, maxlen)
}
