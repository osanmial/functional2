
 -- The program should read and parse the provided JSON config files 
 --         That specify:  1. which address to check. 
 --                                  2. how many times to repeat the check in case of
 --                                                     2.a recoverable failure 
 --                                                     2. b and whether to present the results 
 --                                                          of the checks in color.
 
 --The program should then issue a HEAD request to each address 
 --and see what status code the response contains.
--              1. If the status code indicates a recoverable failure and 
--                  the maximum number of repetitions has not been exhausted, 
--                  the program should try again after a short delay. 
--              2. Otherwise, the program should stop making requests to the address.

-- The program should finally collect the results and print a nicely formatted
--  summary into the terminal.
--               If so requested and supported by the terminal, 
--               the summary should be decorated with bright colors.