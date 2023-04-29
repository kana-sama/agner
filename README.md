Компилятор сабсета эрланга в бинарник. Для образовательных целей.

Для сборки нужен ghc 9.6.1 и hpack
```
hpack
cabal install
```

### Использование
```bash
# собрать в example
agner example.agn -o example
./example

# притипринтинг
agner pretty example.agn
```

# TODO:
- [x] целые числа
- [x] атомы
- [x] функции с патматачем
- [x] ссылки на функции
- [x] кортежи
- [x] списки
- [x] гц
- [x] оптимизация хвостовой рекурсии
- [x] spawn + процессы + шедулер
- [x] receive + send
- [x] строки, литералы чаров
- [x] begin Expr+ end.
- [x] операторы для чисел: +/1, -/1, +/2, -/2, */2, div/2, rem/2
- [x] битовые операторы: bnot/1, band/2, bor/2, bxor/2, bsl/2, bsr/2
- [x] булевы операторы: not/1, and/2, or/2, xor/2
- [x] ленивые операторы: andalso/2, orelse/2
- [x] операторы сравнения: ==, /=, =<, <, >=, >, =:=, =/=
- [x] операторы для списков: ++/2
- [x] case Expr of (Pat -> Exprs)+ end
- [x] гарды
- [x] if
- [x] модули (module, ренейминг)
- [x] рекорды
- [x] мапы
- [ ] битстроки
- [ ] тесты
- [ ] принт рекорда
- [ ] паттерн мапки
- [ ] принт мапы
- [ ] сравнение мапы

на этом этапе можно переписывать компилятор на agner, а потом:
- [ ] external name import
- [ ] стд либа, встроенные функции, IO
- [ ] --/2
- [ ] maybe
- [ ] лямбды
- [ ] opaque
- [ ] оптимизация хвостовых вызовов
- [ ] линтинг: неизвестные переменные и функции, неиспользуемые переменые
- [ ] исключения, ошибки, падения, райзы, линки
- [ ] лист-выражения, мап-выражения, бит-стринг выражения
- [ ] receive Case* after Expr -> Expr+ end
- [ ] раздельная компиляция

# Когда будет нефиг делать:
- [ ] мультикор
- [ ] parse transformations
- [ ] типы
- [ ] сессионные типы для процессов
- [ ] OTP (супервайзеры, серверы, приложения)
- [ ] оптимизации (гц, выхлопа, трансформации кода)


- принты замыканий
- разобраться почему реф из прмера не работает
- правильный вызов замыканий в spawn
- обобщить кодген для функций и лямбд
