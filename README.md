# polynomial-language
 Язык работы с полиномами.
 
## Принципы, заложенные в дизайн языка
1) Есть два типа команд, которые обрабатывает язык: инициализация переменной и вывод на экран. Отделяются эти команды при помощи «;»;
2) Присваивание полинома некоторой переменной (инициализация переменной). <br/>
Имена переменных, которым можно присваивать полиномы, начинаются со знака $ и задаются заглавными буквами латинскими буквами верхнего регистра без посторонних символов. Максимальная длина имени равна 10, если длина больше, то возникает соответствующая ошибка. Всего язык поддерживает 50 различных переменных.

       $AAAB = (-2s+1)*(2s+1)+$BBBA
3) Вывод на экран при помощи print().

       print($DD+x-1)
4) Поддержка однострочных комментариев.

       $AAAB = (-2s+1)*(2s+1) #comment    
5) В программе поддерживаются следующие операции: +, -, *, ^, (). Сокращённое умножение **не** поддерживается.
6) В полиномах используются переменные, заданные латинскими буквами нижнего регистра.
7) Обработка полиномов от двух и более переменных не предусмотрена.

## Обрабатываемые ошибки
1) Дублирование операторов (***duplicate operators.***);
2) Имя переменной, превышающее 10 знаков (***variable name is too long.***);
3) Неизвестный символ (***unknown symbol.***);
4) Пропуск $ при инициализации переменной (***missing '$'.***);
5) Внутри полинома вместо имён переменных использованы имена, предназначенные для хранения полиномов (***using polynomial name for variable name.***);
6) Полином от нескольких переменных /операция производится над полиномами от разных переменных (***there are different variables in the polynomial.***);
7) Слишком много переменных объявлено (***too many variables (maximum 50).***);
8) Степень полинома >100/выполнение операции приводит к полиному степени >100 (***too great a value of the degree.***);
9) 0^0 (неопределённость) (***uncertainty arose (0^0).***);
10) Используется переменная необъявленная ранее (***undeclared variable.***);
11) Полином/число возводится в полином степени >0 (***degree of a variable cannot be a polynomial.***);
12) Полином/число возводится в отрицательную степень (***degree of a variable cannot be a negative number.***);    


## Пример
Пример входного файла *example.txt*.
