package org.cornutum.examples;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MySimpleTest_PostsTest {

    @Test
    public void getPosts_IdsDefined_Is_Yes() {
        given()
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
            .queryParam( "ids", "100|35|31|12")
        .when()
            .request( "GET", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void getPosts_IdsDefined_Is_No() {
        given()
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
            .queryParam( "ids", "-6")
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
            .queryParam( "ids", "0|80|63|32|91")
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
            .queryParam( "ids", "")
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
            .queryParam( "ids", "0|33|92|92")
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
            .header( "X-Post-Types", "2345,7700")
            .header( "X-User-Id", "46782787")
        .when()
            .request( "OPTIONS", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void optionsPosts_XPostTypesItemsContainsValue_Is_7700() {
        given()
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
            .header( "X-User-Id", "1002717876")
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
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "295907700")
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
            .header( "X-Post-Types", "")
            .header( "X-User-Id", "940566730")
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
            .header( "X-Post-Types", "1001")
            .header( "X-User-Id", "995957995")
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
            .header( "X-Post-Types", "1001,2345,7700")
            .header( "X-User-Id", "543574415")
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
            .header( "X-Post-Types", ",7700")
            .header( "X-User-Id", "324488672")
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
            .header( "X-Post-Types", "_0>6l,1001")
            .header( "X-User-Id", "156012952")
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
            .header( "X-Post-Types", "260738342,1001")
            .header( "X-User-Id", "330205658")
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
            .header( "X-Post-Types", "1001,1001")
            .header( "X-User-Id", "287469117")
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
            .header( "X-Post-Types", "1001,7700")
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
            .header( "X-Post-Types", "1001,2345")
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
            .header( "X-Post-Types", "1001,2345")
            .header( "X-User-Id", ";UA")
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
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"m@W.com\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedValue_Is_False() {
        given()
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
            .cookie( "approved", "true")
            .contentType( "application/json")
            .request().body( "{\"text\":\"ezMe2&I.\\\"aMq>yn}=%E|qk,BL`=d\\\\',p6z*[3:hJZwGvOPrjzu61{$ek@KVCB]|8\",\"email\":\"2.6.=.&.?.K.I.s.!@7sq9.96wcX.edu\"}")
        .when()
            .request( "POST", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postPosts_ApprovedDefined_Is_No() {
        given()
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
            .cookie( "approved", "g<h[(2")
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
            .cookie( "approved", "false")
            .contentType( "application/xml")
            .request().body( "{\"yqjvfkkbpw\":-423.0,\"lnyiywnpwriprxz\":-902}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "709.7")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":37.6}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"Jm.gov\"}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"QLv?Bb963Fs.YrQLFE;nR~eE@6w6O.org\"}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"email\":\"2@N.org\"}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":null,\"email\":\"+@A.com\"}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":true,\"email\":\"r@S.edu\"}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\",XM8YB6+me$jhjpa5B#QsXRRyK4s=zT)f_fpwYXqdu21?A] 5t8uZe7$5NG0Wed b\",\"email\":\"z@I.net\"}")
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
            .cookie( "approved", "false")
            .contentType( "application/json")
            .request().body( "{\"text\":\"\",\"email\":\"o@g.gov\",\"oychp\":-638.3,\"iimkfycd\":[],\"kuhhctjnrda\":805}")
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
            .cookie( "country", "I")
            .cookie( "region", "W")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"0@p.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .cookie( "country", ":IEt|~VjQA_R}TA(")
            .cookie( "region", "#:iPPHOumVFtB^@r")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"JFQM3N>B1#lB/U{OHId-;1^nGH1l+ah#,#![^s\\\\brAMrXLB\\\\ u$[V><]\\\"dv*RKO;\",\"email\":\"GJ@W.kds.PHn.7DP.5GF.aNX.OZp.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"R@L.net\"}")
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
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"B@N.org\"}")
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
            .cookie( "postId", "true")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"r@i.net\"}")
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
            .cookie( "region", "h")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"*@3.net\"}")
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
            .cookie( "country", "")
            .cookie( "region", "k")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"l@u.gov\"}")
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
            .cookie( "country", "GF:-Al}%h'$N)k5Nn")
            .cookie( "region", "#")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@O.edu\"}")
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
            .cookie( "country", "b")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"{@V.gov\"}")
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
            .cookie( "country", "[")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"v@S.com\"}")
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
            .cookie( "country", ".")
            .cookie( "region", ")dzW[S-YC=k7?yx?K")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"U@z.gov\"}")
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
            .cookie( "country", "%")
            .cookie( "region", "T")
            .cookie( "fvwfwnxkmrev", "true")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"A@3.edu\"}")
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
            .cookie( "country", "c")
            .cookie( "region", "t")
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
            .cookie( "country", "I")
            .cookie( "region", "~")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "boolean", "true")
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
            .cookie( "country", "O")
            .cookie( "region", "+")
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
            .cookie( "country", "?")
            .cookie( "region", "@")
            .contentType( "text/plain")
            .request().body( "193")
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
            .cookie( "country", "w")
            .cookie( "region", "{")
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
            .cookie( "country", "[")
            .cookie( "region", "?")
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
            .cookie( "country", "@")
            .cookie( "region", "x")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"j6.com\"}")
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
            .cookie( "country", "$")
            .cookie( "region", "l")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"|7Gr.Ptv.A'U@r.Cn3N.o79T.57vS.org\"}")
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
            .cookie( "country", "D")
            .cookie( "region", "s")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"q@L.com\"}")
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
            .cookie( "country", "s")
            .cookie( "region", "n")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"j@4.gov\"}")
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
            .cookie( "country", "Y")
            .cookie( "region", "{")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"S:Y2'%=]f[\\\"jLb$%[]\\\\ae(_<-<MP\\\\f~(Qe)c#9}P`CkQ <[8liE&[lM(raFH&GF3.\",\"email\":\"=@3.org\"}")
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
            .cookie( "country", "h")
            .cookie( "region", "{")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"t@d.gov\",\"mnxgkkxm\":-180}")
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
            .cookie( "postId", "true")
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
            .cookie( "postId", "|B")
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
            .cookie( "postId", "3=rI:rH7jvN+>LNx0M#eAmB3|C")
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
            .cookie( "postId", "A|A")
        .when()
            .request( "TRACE", "/posts")
        .then()
            // postId.Items.Unique=No
            .statusCode( isBadRequest())
            ;
    }

    private Matcher<Integer> isSuccess() {
        return allOf( greaterThanOrEqualTo(200), lessThan(300));
    }

    private Matcher<Integer> isBadRequest() {
        return allOf( greaterThanOrEqualTo(400), lessThan(500));
    }
}
