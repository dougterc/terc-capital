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