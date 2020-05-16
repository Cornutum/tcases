package org.cornutum;

import org.cornutum.utils.MyBaseClass

import org.junit.Test;

import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class MyTest extends MyBaseClass {

    @Test
    public void patchPost_PostMarksDefined_Is_Yes() {
        given()
            .queryParam( "Post Marks", "{X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_3() {
        given()
            .queryParam( "Post Marks", "<Y> #Z {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Z() {
        given()
            .queryParam( "Post Marks", "#Z")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsUnique_Is_No() {
        given()
            .queryParam( "Post Marks", "{X} {X} {X}")
        .when()
            .request( "PATCH", "/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void patchPost_PostMarksDefined_Is_No() {
        given()
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksType_Is_NotArray() {
        given()
            .queryParam( "Post Marks", "-325")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Type=Not array
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_0() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsSize_Is_4() {
        given()
            .queryParam( "Post Marks", "<Y> <Y> <Y> <Y>")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Size=4
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_Null() {
        given()
            .queryParam( "Post Marks", "")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsType_Is_NotString() {
        given()
            .queryParam( "Post Marks", "c")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void patchPost_PostMarksItemsContainsValue_Is_Other() {
        given()
            .queryParam( "Post Marks", "j]s}f?r68%}sCY:`Ox6#0{D4#7]d#G6'ctN&LOh(h8lDPv@KQt$By\"Pe`}7zy\"gjGs6(2SD,;HDm8&5ui)m.gLEXa[@j39#x/n;y2*ql0'1kFor@*kV{LMqYgm}RI|%,?2Xymv&}H#;=Djkkb=FG+};m0{JacOOE\\zCRKR7+~B|)bYS!'/|cFUf1<inkYU1Q;..e}q/L~$R=@u7_(}{<s%`yJ!>P1\\")
        .when()
            .request( "PATCH", "/post")
        .then()
            // Post-Marks.Items.Contains.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_Yes() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_Gt_0() {
        given()
            .queryParam( "postId", "429373621376764523.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "(?)")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_MeYou() {
        given()
            .queryParam( "postId", "0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "true")
            .formParam( "reviewer", "Me+You")
        .when()
            .request( "PUT", "/post")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPost_PostIdDefined_Is_No() {
        given()
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdType_Is_Null() {
        given()
            .queryParam( "postId", "")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdType_Is_NotNumber() {
        given()
            .queryParam( "postId", "mruonbuzblmtih,bnYfUHN\\")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Type=Not number
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_PostIdValue_Is_M1() {
        given()
            .queryParam( "postId", "-1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // postId.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyDefined_Is_No() {
        given()
            .queryParam( "postId", "395657729052207882.7")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyMediaType_Is_Other() {
        given()
            .queryParam( "postId", "178844844465171357.9")
            .contentType( "application/json")
            .request().body( "{\"mhtql\":\")dNI\"}")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.Media-Type=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_Null() {
        given()
            .queryParam( "postId", "384508755815968548.5")
            .contentType( "application/x-www-form-urlencoded")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedType_Is_NotObject() {
        given()
            .queryParam( "postId", "54123914100510963.7")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "0", "F&")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedDefined_Is_No() {
        given()
            .queryParam( "postId", "488307816111880069.0")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_Null() {
        given()
            .queryParam( "postId", "487540343777326758.2")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesApprovedType_Is_NotBoolean() {
        given()
            .queryParam( "postId", "293352798676361038.1")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "200.7")
            .formParam( "reviewer", "Larry Moe")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.approved.Type=Not boolean
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerDefined_Is_No() {
        given()
            .queryParam( "postId", "394806315164229311.8")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_Null() {
        given()
            .queryParam( "postId", "501513882115898758.3")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerType_Is_NotString() {
        given()
            .queryParam( "postId", "24320768166583601.4")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "-171")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesReviewerValue_Is_Other() {
        given()
            .queryParam( "postId", "424633537822095097.2")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "tDB!l|]S.!=6A\\cplTx5Xa;j*!GmV4l&8J_C:'5s_has%$ASU.QipQ,TsI`zu6;ta,j")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.reviewer.Value=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPost_BodyApplicationXWwwFormUrlencodedValuePropertiesAdditional_Is_Yes() {
        given()
            .queryParam( "postId", "831430642366230064.6")
            .contentType( "application/x-www-form-urlencoded")
            .formParam( "approved", "false")
            .formParam( "reviewer", "Larry Moe")
            .formParam( "vzzeuckjfvfj", "sjnzmw,186,ogcopbfnok,613,nb,-974")
        .when()
            .request( "PUT", "/post")
        .then()
            // Body.application-x-www-form-urlencoded.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_Yes() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1000() {
        given()
            .pathParam( "approved", ".1")
            .pathParam( "userId", ".1000")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdDefined_Is_No() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", "")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_Null() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdType_Is_NotInteger() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_M1() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".-1")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=-1
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_UserIdValue_Is_1001() {
        given()
            .pathParam( "approved", ".0")
            .pathParam( "userId", ".1001")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // userId.Value.Is=1001
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedDefined_Is_No() {
        given()
            .pathParam( "approved", "")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_Null() {
        given()
            .pathParam( "approved", ".")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedType_Is_NotInteger() {
        given()
            .pathParam( "approved", "._")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Type=Not integer
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void deletePostUserIdApproved_ApprovedValue_Is_Other() {
        given()
            .pathParam( "approved", ".-660126811")
            .pathParam( "userId", ".0")
        .when()
            .request( "DELETE", "/post/{userId}/{approved}")
        .then()
            // approved.Value.Is=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_Yes() {
        given()
            .cookie( "country", "4")
            .cookie( "region", ">")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@u.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_16() {
        given()
            .cookie( "country", "59/]S|iXKvw{MDZl")
            .cookie( "region", "WT(|.f-V+0l'Eh80")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"[Mm;>7KVj.uk!bMr$S/-*% jTw/P%.T#~a sHY |UzR3*.>'PQE0^Sjhn!14kQLd\",\"email\":\"c.?@ZXlp.ND79.cS0O.a0ol.iCC8.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            .statusCode( allOf( greaterThanOrEqualTo(200), lessThan(300)))
            ;
    }

    @Test
    public void putPosts_PostIdDefined_Is_No() {
        given()
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"=@d.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_Null() {
        given()
            .cookie( "postId", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"7@N.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdType_Is_NotObject() {
        given()
            .cookie( "postId", "Rbl'C6t7")
            .cookie( "postId", ">D3u")
            .cookie( "postId", "|)")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"c@h.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryDefined_Is_No() {
        given()
            .cookie( "region", "+")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"_@3.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_Null() {
        given()
            .cookie( "country", "")
            .cookie( "region", "E")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"`@h.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryType_Is_NotString() {
        given()
            .cookie( "country", "JI7K8,bO*2,")
            .cookie( "region", ")")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"b@D.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_0() {
        given()
            .cookie( "country", "")
            .cookie( "region", "6")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"X@L.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesCountryValueLength_Is_17() {
        given()
            .cookie( "country", "{DMe:+Q(5)aW)N%Ge")
            .cookie( "region", "@")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"J@K.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.country.Value.Length=17
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionDefined_Is_No() {
        given()
            .cookie( "country", "M")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"Q@p.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_Null() {
        given()
            .cookie( "country", "E")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"-@q.edu\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionType_Is_NotString() {
        given()
            .cookie( "country", "7")
            .cookie( "region", "681.2")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"-@H.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_0() {
        given()
            .cookie( "country", "Q")
            .cookie( "region", "")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"}@y.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=0
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesRegionValueLength_Is_17() {
        given()
            .cookie( "country", "/")
            .cookie( "region", "H}eNf[xV6-IJBqH{s")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"g@4.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // postId.Value.Properties.region.Value.Length=17
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_PostIdValuePropertiesAdditional_Is_Yes() {
        given()
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
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyDefined_Is_No() {
        given()
            .cookie( "country", "+")
            .cookie( "region", "w")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyMediaType_Is_Other() {
        given()
            .cookie( "country", "H")
            .cookie( "region", "u")
            .contentType( "text/xml")
            .request().body( "{\"fiwg\":[]}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.Media-Type=Other
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_Null() {
        given()
            .cookie( "country", "b")
            .cookie( "region", "c")
            .contentType( "text/plain")
            .request().body( "null")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainType_Is_NotObject() {
        given()
            .cookie( "country", "0")
            .cookie( "region", "-")
            .contentType( "text/plain")
            .request().body( "[]")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Type=Not object
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailDefined_Is_No() {
        given()
            .cookie( "country", ")")
            .cookie( "region", "'")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_Null() {
        given()
            .cookie( "country", "3")
            .cookie( "region", "]")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":null}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailType_Is_NotString() {
        given()
            .cookie( "country", "N")
            .cookie( "region", "U")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":true}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_6() {
        given()
            .cookie( "country", "}")
            .cookie( "region", "z")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"to.com\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=6
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesEmailValueLength_Is_33() {
        given()
            .cookie( "country", "N")
            .cookie( "region", "O")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"1.==.DZ.=t.ur.'i@9Pe8f.AlmtfC.org\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.email.Value.Length=33
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextDefined_Is_No() {
        given()
            .cookie( "country", ".")
            .cookie( "region", "-")
            .contentType( "text/plain")
            .request().body( "{\"email\":\"l@c.gov\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Defined=No
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_Null() {
        given()
            .cookie( "country", "Z")
            .cookie( "region", "l")
            .contentType( "text/plain")
            .request().body( "{\"text\":null,\"email\":\"2@O.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=null
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextType_Is_NotString() {
        given()
            .cookie( "country", "B")
            .cookie( "region", "N")
            .contentType( "text/plain")
            .request().body( "{\"text\":[\"]h.\",\"[m3'1;\"],\"email\":\"s@O.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Type=Not string
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesTextValueLength_Is_65() {
        given()
            .cookie( "country", "d")
            .cookie( "region", "<")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"qBJlS7?74*T+5maBymiYW9su)2#D*ifT0l%FlO;](w4U,FUY$s_qZdc|-IaFj*/U!\",\"email\":\"r@H.net\"}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.text.Value.Length=65
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }

    @Test
    public void putPosts_BodyTextPlainValuePropertiesAdditional_Is_Yes() {
        given()
            .cookie( "country", "~")
            .cookie( "region", "k")
            .contentType( "text/plain")
            .request().body( "{\"text\":\"\",\"email\":\"$@U.org\",\"inicg\":563,\"z\":154.1}")
        .when()
            .request( "PUT", "/posts")
        .then()
            // Body.text-plain.Value.Properties.Additional=Yes
            .statusCode( allOf( greaterThanOrEqualTo(400), lessThan(500)))
            ;
    }
}
