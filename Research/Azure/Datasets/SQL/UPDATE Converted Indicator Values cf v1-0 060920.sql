
UPDATE dbo.tblCategoryConverted
SET [ECommerceFlag-U] = 
	(CASE
		WHEN (SELECT ECommerceFlag FROM tblCategoryValues) = 'U' THEN 1
		ELSE 0
     END);

UPDATE dbo.tblCategoryConverted
SET [[ECommerceFlag-Y] = 
	(CASE
		WHEN (SELECT ECommerceFlag FROM tblCategoryValues) = 'Y' THEN 1
		ELSE 0
     END);