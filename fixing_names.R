oxygen<-oxygen(scientific_fish_list)

oxygen<-oxygen()

fish_list2 <- validate_names(c("white sucker","yellow perch","lake whitefish","northern pike","walleye","burbot (ling)","lake trout","sucker family","smallmouth bass","sauger","mooneye","brook trout","sturgeon","cisco (lake herring)","black crappie","longnose sucker","blackfin cisco","shortjaw cisco","channel catfish","freshwater drum","white bass","coho salmon","rainbow trout","brown bullhead","common carp","largemouth bass","white perch","brown trout","chinook salmon","common shiner","bigmouth buffalo","rock bass","round whitefish","american eel","rainbow smelt","redhorse sucker","bluegill","pumpkinseed","white crappie","goldfish","siscowet","humper (banker) lake trout","atlantic salmon","pink salmon","bloater","bowfin","chub (not c. artedii)","splake","muskellunge","salmon hybrid","aurora trout","whitefish hybrid","gizzard shad","shorthead redhorse","silver redhorse","goldeye","fallfish","golden redhorse sucker","spotted sucker","golden shiner"))



##contaminants fish list
#"white sucker","yellow perch","lake whitefish","northern pike","walleye","burbot (ling)","lake trout","sucker family","smallmouth bass","sauger","mooneye","brook trout","sturgeon","cisco (lake herring)","black crappie","longnose sucker","blackfin cisco","shortjaw cisco","channel catfish","freshwater drum","white bass","coho salmon","rainbow trout","brown bullhead","common carp","largemouth bass","white perch","brown trout","chinook salmon","common shiner","bigmouth buffalo","rock bass","round whitefish","american eel","rainbow smelt","redhorse sucker","bluegill","pumpkinseed","white crappie","goldfish","siscowet","humper (banker) lake trout","atlantic salmon","pink salmon","bloater","bowfin","chub (not c. artedii)","splake","muskellunge","salmon hybrid","aurora trout","whitefish hybrid","gizzard shad","shorthead redhorse","silver redhorse","goldeye","fallfish","golden redhorse sucker","spotted sucker","golden shiner"
##missing fish once combined with FBtrophic levels
#"white sucker",______________,"lake whitefish","northern pike","walleye",_______________,"lake trout",_______________,"smallmouth bass","sauger","mooneye","brook trout","sturgeon",______________________,"black crappie","longnose sucker","blackfin cisco","shortjaw cisco","channel catfish",_________________,"white bass","coho salmon","rainbow trout","brown bullhead","common carp",_________________,"white perch",_____________,"chinook salmon","common shiner",__________________,"rock bass","round whitefish","american eel","rainbow smelt",_________________,"bluegill","pumpkinseed","white crappie","goldfish",__________,____________________________,"atlantic salmon","pink salmon","bloater",________,_______________________,________,"muskellunge",_______________,______________,__________________,______________,____________________,_________________,"goldeye","fallfish",________________________,________________,"golden shiner"  
##problems
#"sucker family","cisco (lake herring)","freshwater drum","bigmouth buffalo","redhorse sucker","chub (not c. artedii)","splake","salmon hybrid","whitefish hybrid","gizzard shad","shorthead redhorse","silver redhorse","golden redhorse sucker","spotted sucker"
##names fixed
#"yellow perch","burbot (ling)","largemouth bass","brown trout","siscowet","humper (banker) lake trout","bowfin","aurora trout"


fish_Hg_PCB2 <- fish_Hg_PCB %>% 
  mutate(FBname = str_replace_all(FBname, "yellow perch", "american yellow perch"))

fish_Hg_PCB2 <- fish_Hg_PCB2 %>% 
  mutate(FBname = str_replace_all(FBname, "burbot (ling)", "burbot"))

fish_Hg_PCB2 <- fish_Hg_PCB2 %>% 
  mutate(FBname = str_replace_all(FBname, "largemouth bass", "largemouth black bass"))

fish_Hg_PCB2 <- fish_Hg_PCB2 %>% 
  mutate(FBname = str_replace_all(FBname, "brown trout", "sea trout"))

fish_Hg_PCB2 <- fish_Hg_PCB2 %>% 
  mutate(FBname = str_replace_all(FBname, "siscowet", "lake trout"))

fish_Hg_PCB2 <- fish_Hg_PCB2 %>% 
  mutate(FBname = str_replace_all(FBname, "humper (banker) lake trout", "lake trout"))

fish_Hg_PCB2 <- fish_Hg_PCB2 %>% 
  mutate(FBname = str_replace_all(FBname, "bowfin", "freshwater bream"))

fish_Hg_PCB2 <- fish_Hg_PCB2 %>% 
  mutate(FBname = str_replace_all(FBname, "aurora trout", "brook trout"))
