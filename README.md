An OTP application Alarm System simulation.
=====

An OTP application

Features
-----------
- sterowanie systemem za pomocą socketów,
- definiowanie stref oraz przypisywanie do nich wejść,
- definiowanie kodów sterujących systemem,
- załączanie/wyłączanie stref oraz wyłączanie alarmu,
- symulacja pobudzenia czujki oraz sabotaż.

Description
-----
Pamięć alarmu definiowana jest w pliku memory z użyciem poniższych rekordów.

    -record(zone, {name, inputs = []}).
    --opis rekordu code {numer_kodu, stan_w_którym_działa, typ_akcji, parametry_akcji}
    -record(code, {num, state, type, params}). 
    -record(mem, {zones = [], active_zones = [], codes = []}).

Kod może mieć różne działanie w każdym ze stanów.
Typy akcji:
 - activate_zone - włączanie strefy/stref,
 - deactivate_zone - wyłączanie strefy/stref,
 - turn_off - wyłączanie alarmu.
 
System działa z wykorzystaniem zachowania gen_statem. Wyróżniamy trzy stany:
 - idle - alarm jest bezczynny,
 - watch - system jest w stanie czuwania, conajmniej jedna strefa aktywna,
 - alarm_on - alarm włączony, nastąpiło pobudzenie czujki z aktywnej strefy albo sabotaż

Z systemem można połączyć się za pomocą socketów. Służy do tego prosty manipulator napisany w python3. 
Dostępne akcje z poziomu manipulatora:
 - podanie kodu składającego się z cyfr np. '123' - wykonanie jednego typu akcji obsługiwanych przez system,
 - 'sabotage numer_czujki' - sumulacja sabotażu czujki np. 'sabotage 1',
 - 'activate numer_czujki' - symulacja pobudzenia czujki np. 'activate 1',
 - 'restart' - naprawa uszkodzonych czujek podczas sabotażu,
 - 'quit' - rozłączenie manipulatora z systemem.



Usage
----
Starting erlang app:

    $ rebar3 shell
    1> application:start(alarmsys).
    
Starting simple manipulator in python:
    
    $ cd manipulator/
    $ python3 main.py
    

