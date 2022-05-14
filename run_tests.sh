clang -c -pthread -emit-llvm liberate_josh.c
ocamlbuild -pkgs llvm,llvm.bitreader,llvm.linker josh.native
chmod -R a+rX tests
test_files=(`ls tests/*.josh`)
counter=1
echo "---Running Tests for Josh---"
for josh_file in "${test_files[@]}"
do
    first_line=$(head -n 1 $josh_file)
    file_name=${josh_file%.*}
    test_name=${file_name##*/}
    if [[ "$first_line" = "IGNORE" ]]; then
        echo "$counter. $test_name: IGNORE"
        ((counter++))
        continue
    fi
    out_file="$file_name.out"
    ./josh.native -l $josh_file > $out_file 2>&1
    suc_file="$file_name.suc"
    err_file="$file_name.err"
    if [ -f "$suc_file" ]; then
        expected=`cat $suc_file`
        output=`lli $out_file`
        rm $out_file
        if [ "$output" = "$expected" ]
        then
            echo "$counter. $test_name: PASS"
        else
            echo "$counter. $test_name: FAIL\nEXPECTED: $expected\nOUTPUT: $output\nERROR: $error"
        fi
    elif [ -f "$err_file" ]; then
        expected=`cat $err_file`
        output=`cat $out_file`
        rm $out_file
        if [ "$output" = "$expected" ]
        then
            echo "$counter. $test_name: PASS"
        else
            echo "$counter. $test_name: FAIL $output"
            break
        fi
    else
        echo "$counter. $test_name: Missing! Input file with .josh extension found, but expected output file .suc/.err is missing"
        output=`lli $out_file`
        echo "OUTPUT:$output"
        rm $out_file
    fi
    ((counter++))
done