from kirbybase import KirbyBase

db = KirbyBase()
print db.select('/home/micheles/packages/KirbyBase-1.8/plane.tbl',
                ['country','speed'],['USA','>400'])
