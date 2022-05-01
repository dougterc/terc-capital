CREATE TABLE SCREENER
(
  ticker VARCHAR(10) NOT NULL,
  company VARCHAR(255),
  sector VARCHAR(125),
  industry VARCHAR(255),
  country VARCHAR(50),
  date_updated DATE,
  PRIMARY KEY (ticker)
);

CREATE TABLE USER
(
  user_id INT NOT NULL AUTO_INCREMENT,
  user_first VARCHAR(255) NOT NULL,
  user_last VARCHAR(255) NOT NULL,
  user_email VARCHAR(255) NOT NULL,
  user_phone VARCHAR(20),
  user_address VARCHAR(255),
  user_city VARCHAR(255),
  user_state VARCHAR(10),
  user_country VARCHAR(5),
  PRIMARY KEY (user_id)
);

CREATE TABLE PORTFOLIO
(
  port_id BIGINT NOT NULL AUTO_INCREMENT,
  port_name VARCHAR(255),
  date_opened DATETIME,
  date_closed DATETIME,
  user_id INT NOT NULL,
  PRIMARY KEY (port_id),
  FOREIGN KEY (user_id) REFERENCES USER(user_id)
);

CREATE TABLE WATCHLIST
(
  watch_id BIGINT NOT NULL AUTO_INCREMENT,
  port_id BIGINT NOT NULL,
  watch_name VARCHAR(255),
  PRIMARY KEY (watch_id),
  FOREIGN KEY (port_id) REFERENCES PORTFOLIO(port_id)
);

CREATE TABLE WL_ENTITIES
(
    wl_ent_id BIGINT NOT NULL AUTO_INCREMENT,
    watch_id BIGINT NOT NULL,
    ticker VARCHAR(10) NOT NULL,
    PRIMARY KEY (hd_id),
    FOREIGN KEY (ticker) REFERENCES SCREENER(ticker),
    FOREIGN KEY (watch_id) REFERENCES WATCHLIST(watch_id)
);

CREATE TABLE HIST_DATA
(
    hd_id BIGINT NOT NULL AUTO_INCREMENT,
    refdate DATE NOT NULL,
    ticker VARCHAR(10) NOT NULL,
    o_price FLOAT,
    h_price FLOAT,
    l_price FLOAT,
    c_price FLOAT,
    a_price FLOAT,
    c_return FLOAT,
    a_return FLOAT,
    volume INT,
    date_updated DATE,
    PRIMARY KEY (hd_id),
    FOREIGN KEY (ticker) REFERENCES SCREENER(ticker)
);

CREATE TABLE SECURE 
(
  secure_id BIGINT NOT NULL AUTO_INCREMENT,
  user_id INT NOT NULL,
  pswd VARCHAR(255) NOT NULL,
  date_set DATETIME NOT NULL,
  date_exp DATETIME NOT NULL,
  PRIMARY KEY (secure_id),
  FOREIGN KEY (user_id) REFERENCES USER(user_id)
);

CREATE TABLE POSITIONS
(
    pos_id BIGINT NOT NULL AUTO_INCREMENT,
    port_id BIGINT NOT NULL,
    ticker VARCHAR(10) NOT NULL,
    shares INT NOT NULL,
    cost DECIMAL(10,5),
    enter_date DATETIME,
    close_date DATETIME,
    broker_site VARCHAR(255),
    pos_type VARCHAR(10),
    PRIMARY KEY (pos_id),
    FOREIGN KEY (ticker) REFERENCES SCREENER(ticker),
    FOREIGN KEY (port_id) REFERENCES PORTFOLIO(port_id)
);

CREATE TABLE TRADE
(
    trade_id BIGINT NOT NULL AUTO_INCREMENT,
    pos_id BIGINT NOT NULL,
    shares INT NOT NULL,
    trade_price DECIMAL(10,5),
    trade_date DATETIME,
    trade_type VARCHAR(10),
    PRIMARY KEY (trade_id),
    FOREIGN KEY (pos_id) REFERENCES POSITIONS(pos_id)
);

CREATE TABLE RESEARCH
(
  res_id BIGINT NOT NULL AUTO_INCREMENT,
  trade_id BIGINT NOT NULL,
  target_price DECIMAL(10,5),
  target_shares INT,
  buy_sell VARCHAR(5),
  trailing_vol DECIMAL(10,5),
  modelA_type VARCHAR(255),
  modA_proj_price DECIMAL(10,5),
  modA_proj_timeline_days INT,
  target_weight DECIMAL(10,5),
  pairing_stocks VARCHAR(255),
  PRIMARY KEY (res_id),
  FOREIGN KEY (trade_id) REFERENCES TRADE(trade_id)
);