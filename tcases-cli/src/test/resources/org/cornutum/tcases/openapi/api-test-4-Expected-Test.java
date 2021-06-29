package org.cornutum;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyTest extends MyBaseClass {

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "0")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_4() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "100|19|77|63")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "245.3")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_0() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsSize_Is_5() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "0|47|30|89|75")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Size=5
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "qmyumozb,true")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "-1")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsContainsValue_Is_101() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "101")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Contains.Value.Is=101
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void getPosts_IdsItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "ids", "0|96|96|42")
        .when()
            .request( "GET", "/posts")
        .then()
            // ids.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_2345() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "2345,7700")
            .header( "X-User-Id", "761295545")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "7700,2345")
            .header( "X-User-Id", "0")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "X-User-Id", "433704959")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "791204254")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "647")
            .header( "X-User-Id", "249283126")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_1() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001")
            .header( "X-User-Id", "796819282")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,7700,2345")
            .header( "X-User-Id", "837031824")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", ",1001")
            .header( "X-User-Id", "1012855669")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "PyCeS/;&,XTzA Y,~H|,1001")
            .header( "X-User-Id", "944060091")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "-698622611,1001")
            .header( "X-User-Id", "633311474")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Contains.Value.Is=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,1001")
            .header( "X-User-Id", "57695003")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-Post-Types.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,2345")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdType_Is_NotInteger() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", "=6^")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Type=Not integer
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void optionsPosts_XUserIdValue_Is_M1() {
        given()
            .baseUri( forTestServer())
            .header( "X-Post-Types", "1001,7700")
            .header( "X-User-Id", "-1")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            // X-User-Id.Value.Is=-1
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"u@g.org\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_32() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"1ed1Nxq)H0slF@@JRxuM3?G~M\\\\FI.z='r(`zRs2pH)V_iv.])X|$tNs#m@1P[qa[\",\"email\":\"~}S@qaD.1g.Ho.Dk.yZ.Rh.PS.SW.gov\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "")
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_ApprovedType_Is_NotBoolean() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "382")
        .when()
            .request( "POST", "/posts")
        .then()
            // approved.Type=Not boolean
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "number", "575.9")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "null")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "true")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":[\"}NW<_9\",\"lvFe|\"]}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_6() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"3R.com\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesEmailValueLength_Is_33() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"*o.V&.bZ.7~@4rKu4pyQ.7OLQV6uc.net\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"email\":\"R@9.org\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":null,\"email\":\"M@1.gov\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextType_Is_NotString() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":379,\"email\":\"E@y.edu\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Type=Not string
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesTextValueLength_Is_65() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\">P:{i{P<p9[xFbJY9,eFXL|:SO_Wu*^j\\\\gnJl:wY!hf\\\\wyqn|nQ`/cJsTl4qE];Ot\",\"email\":\"J@i.com\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postPosts_BodyApplicationJsonValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"/@V.org\",\"vdma\":758.8,\"nwuxvolpzckttm\":true,\"wpu\":[\"!o:hE\",\"0Q\",\"e \"]}")
        .when()
            .request( "POST", "/posts")
        .then()
            // Body.application-json.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "R")
            .cookie( "region", "3")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"K@a.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "/]S|iXKvw{MDZlWT")
            .cookie( "region", "(|.f-V+0l'Eh80Z=")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"m;>7KVj.uk!bMr$S/-*% jTw/P%.T#~a sHY |UzR3*.>'PQE0^Sjhn!14kQLdhY\",\"email\":\"l_.R{.hD.{7.Hy.$A@a0.oli.CC8.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@d.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"7@N.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "Rbl'C6t7")
            .cookie( "postId", ">D3u")
            .cookie( "postId", "|)")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"c@h.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "region", "+")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"_@3.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "")
            .cookie( "region", "E")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"`@h.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "{DMe:+Q(5)aW)N%Ge")
            .cookie( "region", "@")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"J@K.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "M")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"Q@p.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "E")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"-@q.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "/")
            .cookie( "region", "H}eNf[xV6-IJBqH{s")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"g@4.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=17
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "$")
            .cookie( "region", "q")
            .cookie( "skyxfntetzcmk", "-60.4")
            .cookie( "notextl", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"1@V.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "+")
            .cookie( "region", "w")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "H")
            .cookie( "region", "u")
            .contentType( "text/xml")
            .request().body( "{\"fiwg\":[]}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "b")
            .cookie( "region", "c")
            .contentType( "text/plain")
            .request().body( "null")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "0")
            .cookie( "region", "-")
            .contentType( "text/plain")
            .request().body( "[]")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", ")")
            .cookie( "region", "'")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "3")
            .cookie( "region", "]")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "}")
            .cookie( "region", "z")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"to.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=6
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "N")
            .cookie( "region", "O")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"1.==.DZ.=t.ur.'i@9Pe8f.AlmtfC.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=33
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", ".")
            .cookie( "region", "-")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"l@c.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "Z")
            .cookie( "region", "l")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"2@O.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "d")
            .cookie( "region", "<")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"qBJlS7?74*T+5maBymiYW9su)2#D*ifT0l%FlO;](w4U,FUY$s_qZdc|-IaFj*/U!\",\"email\":\"r@H.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Value.Length=65
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "country", "~")
            .cookie( "region", "k")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"$@U.org\",\"inicg\":563,\"z\":154.1}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.Additional=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_2() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "B|C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_C() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "C")
        .when()
            .request( "TRACE", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void tracePosts_PostIdDefined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdType_Is_NotArray() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "mnl_T[k")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Type=Not array
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_0() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=0
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsSize_Is_3() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "A|C|B")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Size=3
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsContainsValue_Is_Other() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "&8ZJVDR!8L`95Z/C{-+Lv'ODNFkc&/#*@z7ScA_&Kh}c2dxrBz/2PfD*IPF_(d]lYR0q@rqRSby8?k+)tXEc415q-u0`?]oJRBh1*'Rn`I[3}3H*wvowcHY$V$ySBh*/k(0hA<SRbfNgrv8:-cfSWPu4ZCLv@4@u*0ii@bn@h>cW)vV~(<Q#]N3fX#|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Contains.Value=Other
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void tracePosts_PostIdItemsUnique_Is_No() {
        given()
            .baseUri( forTestServer())
            .cookie( "postId", "A|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    private static Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private static Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }

    private static String forTestServer() {
        return forTestServer( null);
    }

    private static String forTestServer( String defaultUri) {
        String testServer = tcasesApiServer();
        return
            defaultUri == null || !testServer.isEmpty()
            ? testServer
            : defaultUri;
    }

    private static String tcasesApiServer() {
        String uri = System.getProperty( "tcasesApiServer");
        return uri == null? "" : uri.trim();
    }
}
