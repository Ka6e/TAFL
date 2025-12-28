Feature: Is prime
  Computes is prime number.

  Scenario: User enters a number and program prints is prime number or not
    Given I enter into the console:
      | Value |
      | 17    |
    When I execute the program:
      """
      module Main
      func isPrime(x: int): bool {
        if x <= 1 then {
            return false;
        }

        if x == 2 then {
            return true;
        }

        if x % 2 == 0 then {
            return false;
        }

        var devider = 3;
        while(devider * devider <= x){
            if x % devider == 0 then {
                return false;
            }

            devider = devider + 2;
        }

        return true;
      }

      var x = readNumber();
      var result = isPrime(x);

      if result  then {
        print("number is prime");
      }
      else {
        print("number is not prime");
      }
      """
    Then I should get the output:
      """
      number is prime
      """
