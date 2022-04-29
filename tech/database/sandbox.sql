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

CREATE TABLE USER
(
  user_id INT NOT NULL AUTO_INCREMENT,
  user_first VARCHAR(255) NOT NULL,
  user_last VARCHAR(255) NOT NULL,
  user_email VARCHAR(255) NOT NULL,
  user_phone VARCHAR(20),
  user_address VARCHAR(255),
  user_city VARCHAR(255),
  user_state VARCHAR(10)
  user_country VARCHAR(5)
  PRIMARY KEY (user_id)
)

CREATE TABLE SECURE 
(
  secure_id BIGINT NOT NULL AUTO_INCREMENT,
  user_id INT NOT NULL,
  pswd VARCHAR(255) NOT NULL,
  date_set DATETIME NOT NULL,
  date_exp DATETIME NOT NULL,
  PRIMARY KEY (secure_id),
  FOREIGN KEY (user_id) REFERENCES USER(user_id)
)

CREATE TABLE WATCHLIST
(
  watch_id BIGINT NOT NULL AUTO_INCREMENT,
  user_id INT NOT NULL,
  watch_name VARCHAR(255),
  FOREIGN KEY (user_id) REFERENCES USER(user_id)
)

CREATE TABLE WL_ENTITIES
(
    wl_ent_id BIGINT NOT NULL AUTO_INCREMENT,
    watch_id BIGINT NOT NULL,
    ticker VARCHAR(10) NOT NULL,
    user_id INT NOT NULL,
    PRIMARY KEY (hd_id),
    FOREIGN KEY (ticker) REFERENCES SCREENER(ticker),
    FOREIGN KEY (user_id) REFERENCES USER(user_id)
);