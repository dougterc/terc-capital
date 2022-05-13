/*Get all portfolios for a certain user*/
SELECT *
FROM PORTFOLIO
WHERE user_id = [USERID];
/*----------------------------------------*/

/*Get current portfolio positions for a user id*/
SELECT *
FROM POSITIONS 
    LEFT JOIN PORTFOLIO ON POSITIONS.port_id = PORTFOLIO.port_id
WHERE user_id = [USERID];
/*----------------------------------------*/

/*Get research for all trades based on a user id*/
SELECT *
FROM RESEARCH
    LEFT JOIN TRADE ON RESEARCH.trade_id = TRADE.trade_id
    LEFT JOIN POSITIONS ON TRADE.pos_id = POSITIONS.pos_id
    LEFT JOIN PORTFOLIO ON POSITIONS.port_id = PORTFOLIO.port_id
WHERE user_id = [USERID];
/*----------------------------------------*/

/*Get cost basis of user and portfolio*/
SELECT ticker, (shares * cost) AS 'CostBasis'
FROM POSITIONS 
    LEFT JOIN PORTFOLIO ON POSITIONS.port_id = PORTFOLIO.port_id
WHERE user_id = 3;

SELECT SUM(CostBasis)
FROM (SELECT ticker, (shares * cost) AS 'CostBasis'
FROM POSITIONS 
    LEFT JOIN PORTFOLIO ON POSITIONS.port_id = PORTFOLIO.port_id
WHERE user_id = 3) AS BY_STOCK;

SELECT ticker, (shares * cost) AS 'Cost Basis'
FROM POSITIONS 
    LEFT JOIN PORTFOLIO ON POSITIONS.port_id = PORTFOLIO.port_id
WHERE port_id = 2;

/*----------------------------------------*/

/*Get current portfolio positions for a user id*/

/*----------------------------------------*/