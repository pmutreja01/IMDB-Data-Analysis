from urllib.request import urlopen
from bs4 import BeautifulSoup
import pdb
import re
import time
import random
#url = urlopen("http://www.imdb.com/chart/moviemeter?ref_=nv_mv_mpm_8")
#soup = BeautifulSoup(url,"lxml")
def get_person_name_id_from_url(url):
        # sample imdb person url: http://www.imdb.com/name/nm0000338/?ref_=tt_ov_dr
        # we need to return "nm0000338"
        if url is None:
            return None
        return re.search("(nm[0-9]{7})", url).group()
def get_likes(url):
   try:
       content = urlopen(url).read()
       soup = BeautifulSoup(content, "lxml")
       sentence = soup.find_all(id="u_0_2")[0].span.string # get sentence like: "43K people like this"
       num_likes = sentence.split(" ")[0]
   except Exception as e:
       num_likes = None
   return num_likes
def get_movie_id_from_url(url):
    # sample imdb movie url: http://www.imdb.com/title/tt0068646/?ref_=nv_sr_1
     # we need to return "tt0068646"
    if url is None:
        return None
    return re.search("(tt[0-9]{7})", url).group()

def getMovieDetails(title,url,myFile):
    movie_url = url
    #print (movie_url)

    url1 = urlopen("http://"+url)

    soup = BeautifulSoup(url1,"lxml")
    try:
        director_info = soup.findAll("span",{"itemprop":"director"})
    except:
        director_info=""
    try:
        writer_info = soup.findAll("span",{"itemprop":"creator"})
    except:
        writer_info=""
    try:
        actor_info = soup.findAll("span",{"itemprop":"actors"})
    except:
        actor_info=""
    try:
        release_yr = soup.findAll("span",{"id":"titleYear"})
    except:
        release_yr=""
    try:
        release_date = soup.find("div",{"class":"subtext"}).find("a",{"title":"See more release dates"}).get_text()
    except:
        release_date=""
    try:
        duration = (soup.findAll("time",{"itemprop":"duration"})[1]).get_text().split(' ')[0]
    except:
        duration=""
    #soup.find("div",{"class":"subtext"}).find("time",{"itemprop":"duration"}).get_text()
    try:
        genre_info = soup.findAll("div",{"itemprop":"genre"})
    except:
        genre_info=""
    try:
        rating_val = soup.find("div",{"class":"ratingValue"}).find("span",{"itemprop":"ratingValue"}).get_text()
    except:
        rating_val=""
    try:
        user_rating_count = soup.find("span",{"itemprop":"ratingCount"}).get_text()
    except:
        user_rating_count=""
    try:
        monetry_info = soup.findAll("div",{"class":"txt-block"})
    except:
        monetry_info=""
    try:
        reviews = soup.findAll("span",{"class":"subText"})
    except:
        reviews=""
    try:
        plotwords_url = "http://www.imdb.com"+soup.find("nobr").find("a").get("href")
        url_kw = urlopen(plotwords_url)
        soup_kw = BeautifulSoup(url_kw,"lxml")
        more_kw = soup_kw.findAll("div",{"class":"sodatext"})
    except:
        more_kw=""
    release_year=""
    for ry in release_yr:
        try:
            release_year=(ry.find("a").get_text().strip())
        except:
            release_year=""
    directors=[]
    director_likes=[]
    for di in director_info:
        try:
            directors.append(di.find("span").get_text().strip())
        except:
            directors=""
        try:
            entity_id = get_person_name_id_from_url("www.imdb.com"+di.find("a").get("href"))
            url = "https://www.facebook.com/widgets/like.php?width=280&show_faces=1&layout=standard&href=http%3A%2F%2Fwww.imdb.com%2Fname%2F{}%2F&colorscheme=light".format(entity_id)
            director_likes.append(get_likes(url))
        except:
            director_likes=""

    #print("actors:")
    actors=[]
    actor_likes=[]
    for ac in actor_info:
        try:
            actors.append(ac.find("span").get_text().strip())
        except:
            actors=""
        try:
            entity_id = get_person_name_id_from_url("www.imdb.com"+ac.find("a").get("href"))
            url = "https://www.facebook.com/widgets/like.php?width=280&show_faces=1&layout=standard&href=http%3A%2F%2Fwww.imdb.com%2Fname%2F{}%2F&colorscheme=light".format(entity_id)
            actor_likes.append(get_likes(url))
        except:
            actor_likes=""
    movie_likes = ""
    try:
        entity_id = get_movie_id_from_url(movie_url)
        url = "https://www.facebook.com/widgets/like.php?width=280&show_faces=1&layout=standard&href=http%3A%2F%2Fwww.imdb.com%2Ftitle%2F{}%2F&colorscheme=light".format(entity_id)
        movie_likes=get_likes(url)
        if movie_likes==None:
            movie_likes=""
    except:
        movie_likes=""
    rev = []
    for r in reviews:
        data = r.findAll("a")
        if len(data)!=0:
            rev = data
    try:
        user_reviews = rev[0].get_text().split(" ")[0]
    except:
        user_reviews=""
    try:
        critic_reviews = rev[1].get_text().split(" ")[0]
    except:
        critic_reviews=""
    genres = []
    for g in genre_info:
        data = g.findAll("a")
        if len(data)!=0:
            genres = data
    final_genres=[]
    for gr in genres:
        try:
            final_genres.append(gr.get_text().strip())
        except:
            final_genres=""
    #print("plot keywords:")
    words = []
    for kw in more_kw:
        try:
            data = kw.find("a")
            words.append(data.get_text().strip())
        except:
            words = ""
	#print(words)
    res_mon = []
    budget, gross_income, country, language = "","","",""
    for res in monetry_info:

    #     print(res.text)
    #     print("---------------------------------------")
        temp_str=res.text.replace("\n","")
        if "Budget" in temp_str:
            try:
                budget = (temp_str.split(':')[1]).strip().split(" ")[0]
            except:
                budget=""
        elif "Gross" in temp_str:
            try:
                gross_income = (temp_str.split(':')[1]).strip().split(" ")[0]
            except:
                gross_income=""
        elif "Country" in temp_str:
            try:
                country = (temp_str.split(':')[1]).strip().split(" ")[0]
            except:
                country=""
        elif "Language" in temp_str:
            try:
                language = (temp_str.split(':')[1]).strip().split(" ")[0]
            except:
                language=""
    try:
        cr = soup.findAll(attrs={"itemprop":"contentRating"})
        content_rating = cr[0]['content']
    except:
        content_rating=""
    try:
        final_string=str(directors)+"\t"+critic_reviews.strip()+"\t"+duration.strip()+"\t"+str(director_likes)+"\t"+str(actors)+"\t"+str(actor_likes)+"\t"+gross_income.strip()+"\t"+str(final_genres)+"\t"+title.strip()+"\t"+user_rating_count.strip()+"\t"+str(words)+"\t"+movie_url.strip()+"\t"+user_reviews.strip()+"\t"+language.strip()+"\t"+country.strip()+"\t"+content_rating.strip()+"\t"+budget.strip()+"\t"+release_date.strip()+"\t"+rating_val.strip()+"\t"+movie_likes.strip()+"\n"
    except:
        print (title, movie_url)
    #finalString=title.strip()+"\t"+str(directors)+"\t"+str(director_likes)+"\t"+str(writers)+"\t"+str(actors)+"\t"+str(actor_likes)+"\t"+str(final_genres)+"\t"+str(words)+"\t"+release_year.strip()+"\t"+release_date.strip()+"\t"+duration.strip()+"\t"+rating_val.strip()+"\t"+user_rating_count.strip()+"\t"+country.strip()+"\t"+budget.strip()+"\t"+opening_weekend.strip()+"\t"+gross_income.strip()+"\n"
    #print (finalString)
    myFile.write(final_string)

myFile=open("IMDBdata_resolving_budget_gross_2.txt", "a")
f=open('movieLinks.txt','r')
links=f.readlines()
for link in links:
    getMovieDetails("movie title",link,myFile)


#print(movie_dict)
#print (len(movie_dict))
