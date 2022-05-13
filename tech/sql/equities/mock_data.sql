/*we need user zip*/

/*USER*/
INSERT INTO `terc-capital`.`USER` (`user_first`, `user_last`, `user_email`, `user_phone`, `user_address`, `user_city`, `user_state`, `user_country`) VALUES ('Douglas', 'Terc', 'douglasaterc@gmail.com', '631-987-1947', '650 E Stonewall St.', 'Charlottte', 'NC', 'USA');
INSERT INTO `terc-capital`.`USER` (`user_first`, `user_last`, `user_email`, `user_phone`, `user_address`, `user_city`, `user_state`, `user_country`) VALUES ('Daniel', 'Singleton', 'dannysingleton195@yahoo.com', '703-627-3799', '350 E Stonewall St.', 'Charlottte', 'NC', 'USA');

/*PORTFOLIO*/
INSERT INTO `terc-capital`.`PORTFOLIO` (`port_id`, `port_name`, `date_opened`, `user_id`) VALUES ('1', 'Main Portfolio', '2022-05-11', '3');
INSERT INTO `terc-capital`.`PORTFOLIO` (`port_id`, `port_name`, `date_opened`, `user_id`) VALUES ('2', 'Penny Stocks', '2022-05-11', '3');
INSERT INTO `terc-capital`.`PORTFOLIO` (`port_id`, `port_name`, `date_opened`, `user_id`) VALUES ('3', 'Danny*s Portfolio', '2022-05-11', '4');

/*POSITIONS*/
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('1', 'AAPL', '10', '150.45', '2022-05-03', 'TDA', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('1', 'MSFT', '20', '120.19', '2022-05-04', 'TDA', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('1', 'AEF', '100', '15.57', '2022-05-05', 'TDA', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('1', 'TLSA', '1', '1285.34', '2022-05-06', 'TDA', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('2', 'TRVN', '500', '0.5234', '2022-05-04', 'TDA', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('2', 'CREX', '1000', '0.345', '2022-05-05', 'TDA', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('3', 'PYPL', '10', '220.05', '2022-04-21', 'ETRD', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('3', 'CROX', '55', '50.45', '2022-04-27', 'ETRD', 'Equity');
INSERT INTO `terc-capital`.`POSITIONS` (`port_id`, `ticker`, `shares`, `cost`, `enter_date`, `broker_site`, `pos_type`) VALUES ('3', 'LULU', '14', '156.55', '2022-04-28', 'ETRD', 'Equity');