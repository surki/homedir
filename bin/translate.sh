#!/bin/sh -e

url='http://translate.google.com/translate_t'
data=`echo $* | urlencode`
postdata='hl=en&ie=UTF8&text='"$data"'&langpair=fi|en'
useragent='User-Agent: Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.11) Gecko/2009060308'

# wget -d --post-data "$postdata" -U "$useragent" "$url" -O -
wget -q --post-data "$postdata" -U "$useragent" "$url" -O - |
   egrep -o '<div id=result_box dir="ltr">[^</>]*<\/div>' |
   awk -F'<[^>]*>' '{ print $2 }' | html2text

