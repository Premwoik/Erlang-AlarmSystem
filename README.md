

An OTP application - Alarm System simulation. ![alt text](https://travis-ci.com/Premwoik/Erlang-AlarmSystem.svg?token=6NPxvdxVXjtv553szhtp&branch=develop)
=====

An OTP application which simulate Alarm System. Created by Daniel Stefanik and Przemysław Wojtasik.

Features
-----------
- sterowanie systemem za pomocą socketów,
- definiowanie stref oraz przypisywanie do nich wejść,
- definiowanie kodów sterujących systemem,
- załączanie/wyłączanie stref oraz wyłączanie alarmu,
- symulacja pobudzenia czujki oraz sabotaż.

Description
-----
System można konfigurować w pliku alarm_config.hrl. Podstawowa konfiguracja tworzy 20 czujek, które
mają numery od 1 do 20. Każda zmiana stanu systemu jest obsługiwana przez gen_event, który ma dodane
trzy event handlery tj. wyświetlenie informacji na ekranie, wysłanie powiadomienia do manipulatorów,
zapisanie informacji do pliku.

Format powiadomień:

    -record(noti, {state = null, reason = null, more = null}).
     {noti, State, Reason, More}
 - state - stan do którego przeszedł system po wykonaniu akcji,
 - reason - powód zmiany stanu (wykonana akcja),
 - more - dodatkowe informacje.

Pamięć alarmu definiowana jest w pliku memory z użyciem poniższych rekordów.
    
    -type inputs() :: [integer()].
    -record(zone, {name :: atom(), inputs ::inputs()}).
    
    --opis rekordu code {numer_kodu, stan_w_którym_działa, typ_akcji, parametry_akcji}
    -record(code, {num :: integer(), state :: atom(), type :: atom(), params :: term()}).
    
    -type codes() :: [#code{}].
    -type zones() :: [#zone{}].
    -record(mem, {zones :: zones(), active_zones :: [atom()], codes :: codes()}).

System działa z wykorzystaniem zachowania gen_statem. Wyróżniamy trzy stany:
 - idle - alarm jest bezczynny,
 - watch - system jest w stanie czuwania, co najmniej jedna strefa aktywna,
 - alarm_on - alarm włączony, nastąpiło pobudzenie czujki z aktywnej strefy albo sabotaż.

Każdy kod może mieć różne działanie w każdym ze stanów.
Dostępne typy działań:
 - activate_zone - włączanie strefy/stref (stan idle),
 - deactivate_zone - wyłączanie strefy/stref (stan watch),
 - toggle_zone - przełączanie strefy/stref (stan watch)
 - turn_off - wyłączanie alarmu (stan alarm_on),
 - sabotage_input - sygnalizuje sabotaż wejścia (każdy stan),
 - active_input - sygnalizuje pobudzenie wejscia (stan watch i idle),
 - handle_code - obsługa kodu (każdy stan).
 






Usage
----

Z systemem można połączyć się za pomocą socketów. Służy do tego prosty manipulator napisany w python3. 
Dostępne akcje z poziomu manipulatora:
 - podanie kodu składającego się z cyfr np. '123' - wykonanie jednego typu akcji obsługiwanych przez system,
 - 'sabotage numer_czujki' - symulacja sabotażu czujki np. 'sabotage 1',
 - 'activate numer_czujki' - symulacja pobudzenia czujki np. 'activate 1',
 - 'restart' - naprawa uszkodzonych czujek podczas sabotażu,
 - 'quit' - rozłączenie manipulatora z systemem.


Obsługa bez użycia manipulatora - komendy do użycia w shellu:
 - alarm_core:handle_code(kod) - wykonianie akcji,
 - alarm_inputs:sabotage_sensor(numer_sensora) - sabotaż czujki,
 - alarm_inputs:activate_sensor(numer_sensora) - pobudzenie czujki,
 - alarmsys_sup:restart_inputs() - naprawa wejść.

Przykładowo zdefiniowana pamięć - dostępne kody:   
    
    '123' - załączanie/wyłączanie stref z1 i z2
    '13' - załączanie/wylączanie strefy z3
    '321' - wyłączanie alarmu

Starting erlang app:

    $ rebar3 shell
    1> application:start(alarmsys).
    
Starting simple manipulator in python:
    
    $ cd manipulator/
    $ python3 main.py
    

