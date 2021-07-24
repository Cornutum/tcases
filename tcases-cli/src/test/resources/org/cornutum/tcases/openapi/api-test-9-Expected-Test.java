package org.cornutum.readonly;


import org.junit.Test;

import org.hamcrest.Matcher;
import static io.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

public class ReadOnlyTest extends MyBaseClass {

    @Test
    public void postObject_Param0Defined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "d")
            .queryParam( "delta", "9")
            .queryParam( "uybesnzjgkhhxsw", "; Cq-g>")
            .queryParam( "eufcxhrk", "")
            .queryParam( "khnrjuvnuyk", "589")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0Defined_Is_No() {
        given()
            .baseUri( forTestServer())
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "RfBZ(8DzeQZPj=k.r\\#*Kx/+u -0HE\"?&buS(c6qP~elSQr.el.yIbq<f:oyE=1N *()q8 ;gf=}K.Rx*=2:,N*aKO\"")
            .queryParam( "zmix", "PRBj]X+")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoValueLength_Is_Gt_1() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "<k<r?4IBML~>,GI!wPYs*p.Q85z_-M\"krlKsIbRvN2 ]-O?y<~Yx$Td2/0iX6=q(a%$A9JEKNZ3/[9V9P$ mrtt~65VRmV^O)gcx]wBcJ2MA91&Z8xtl1F-l<E{-~NtTaa({6")
            .queryParam( "delta", "{")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaDefined_Is_No() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "A")
            .queryParam( "dzlzpxgtiwyxjkyo", "")
        .when()
            .request( "POST", "/object")
        .then()
            .statusCode( isSuccess())
            ;
    }

    @Test
    public void postObject_Param0Type_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "param0", "")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0Type_Is_NotObject() {
        given()
            .baseUri( forTestServer())
            .queryParam( "param0", "313.0")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Type=Not object
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertyCount_Is_Lt_2() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "'9vB{A6OgRfX%\"<m]y>zqN;]{ry")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Property-Count=< 2
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesAlphaDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "alpha", "K%/yT&dzxE\\ZnvtWz8.Zi;:$6%K6I} :0SdB]ThT9I$5xOX7S$zW|ShV(C]BTOCBUi]T\">0KaKkBt|Ba/ `,hs6rWB~5^l7Uy~rszs')!IKs~s=[e6&E<}zv^@T7IM7,0!TST;J[Q.ItD<G<dx)f[`6H:CN{\"\"7:L<r9ZYT/P?d8H:&xs[Tbb|{!FEYGLOG -Ni:&R5s.sj<1tudd|9VJ#s,M*|w:;aJ@D!G)")
            .queryParam( "delta", "3SqU;jx7NOVYIK\\Y>/c- 4?{hL_I7n{S+>t=!)K:z!GAcw\\0nQH!p9'ufrlPHeDEJ^|<nIj(yuELM(}pl_Uh{+u7}V-Q6[JU7[mvN;EFk73>[[+_h,Dh}WugcX1(mpOqw>Rhj9yBtS_}evXlt^{d&1^Zi !yT178MaO#We]d9haGayWAQ,}g7BaA5if_+0lh(DR<m|p\"XO?RaeoY*aG9EO_l42z")
            .queryParam( "rrsoxrixlofepmb", "412.6")
            .queryParam( "grptsraygyfeq", "#Ysx~tx")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.alpha.Defined=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesBravoType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "bravo", "")
            .queryParam( "delta", "L)9<-Ef]&(cpJ@61T8u;Df${B<(X*cS(?1Dn;-2^4jmw[m5@in3i0Y~>hF,LA:+7;J?z5'WP?5=Z41PH`y_g`-hCEPj}R$<UI&!EDyMa*kg?k4-ZSP.0$~ *xP[u> jtcwKbuek)fH1\"|Ev[(!'Aq_L)nW]vj\"D~3gVQ?Y:gtlzb-a=-_l<(Q%=z%aD<MIjSl%R@")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.bravo.Type=null
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesCharlieDefined_Is_Yes() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "[SB'qsV(sBk@T$UbOAvM\".x(FO<6<A$-REfB5?@4[VM(qL;es\")Q4TQ'jjt!+^VU$6|]FN!?Hz4/Mdc1qQ2;r8!ft\\Swj:2&^7Y\\&6-~1HSjg,PBJtUq.[z8FW[G\"%L6=<Uh$Zc#_^d1}Ly+iy(%'m<y10[mCLVEI6JPO%RJEO`l_7*=<TRTH\\zz#xHa/C9cfw}?l@DS;@V&>ce}]1MT^")
            .queryParam( "charlie", "y{3k@hGLQ}[!}nNWQKmK\"k(L?}l=bve$Sjc|0mac]&:|a/ymoXbc-6&s[*Z<Pz4Nv!NeGN&mWX7g7GoTR>hFAeH/fya):IB\" K* *}aa0trFafK5Bv{`")
            .queryParam( "vizv", "")
            .queryParam( "dxs", ",+Mkae2+c,1")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.charlie.Defined=Yes
            .statusCode( isBadRequest())
            ;
    }

    @Test
    public void postObject_Param0ValuePropertiesDeltaType_Is_Null() {
        given()
            .baseUri( forTestServer())
            .queryParam( "delta", "")
            .queryParam( "qmtcwaeskvsz", "349.9")
            .queryParam( "equvwto", "-66")
        .when()
            .request( "POST", "/object")
        .then()
            // param0.Value.Properties.delta.Type=null
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
