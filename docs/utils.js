export function fmt_pct(x, digits=0) {
    if (x <= 0.01)
        return "<1%";
    else if (x > 0.99)
        return ">99%";
    else
        return (100*x).toFixed(digits) + "%";
};

export function rate(x) {
    if (x > 0.98) {
        return "Safe Dem."
    } else if (x > 0.8) {
        return "Likely Dem."
    } else if (x > 0.6) {
        return "Lean Dem."
    } else if (x > 0.4) {
        return "Tossup"
    } else if (x > 0.2) {
        return "Lean Rep."
    } else if (x > 0.02) {
        return "Likely Rep."
    } else {
        return "Safe Rep."
    }
}

export function fmt_surname(x) {
    if (x === null) return x;
    return x
        .replace("A. CORTEZ", "A. OCASIO-CORTEZ")
        .replace("M. MEEKS", "M. MILLER-MEEKS")
        .replace("B. DUYNE", "B. VAN DUYNE")
        .replace("C. RODGERS", "C. MCMORRIS RODGERS")
        .replace("S. DELBENE", "S. DelBENE")
        .replace("J. SKINNER", "J. MCLEOD-SKINNER")
        .replace("D. SCHULTZ", "D. WASSERMAN SCHULTZ")
        .replace(/ MC([A-Z])/, " Mc$1")
        .replace(/ DE([SLG])([AEIOU])/, " De$1$2")
        .replace(/ DI([SLG])([AEIOU])/, " Di$1$2");
}

let addImageProcess = function(src) {
    return new Promise((resolve, reject) => {
        let img = new Image()
        img.onload = () => resolve(img)
        img.onerror = reject
        img.src = src
    })
};

export async function load_image(url) {
    const img = await addImageProcess(url);

    let canvas = document.createElement("canvas");
    const h = (canvas.height = img.height);
    const w = (canvas.width = img.width);

    let ctx = canvas.getContext("2d");
    ctx.drawImage(img, 0, 0);
    let raw = ctx.getImageData(0, 0, w, h);

    let out = new Float32Array(w*h);
    for (let i = 0; i < w*h; i++) {
        out[i] = raw.data[4*i] / 256.0;
    }

    out.h = h;
    out.w = w;

    return out;
}



export const AP = {AK: "Alaska", AL: "Ala.", AR: "Ark.", AZ: "Ariz.", CA: "Calif.",
    CO: "Colo.", CT: "Conn.", DC: "D.C.", DE: "Del.", FL: "Fla.", GA: "Ga.",
    HI: "Hawaii", IA: "Iowa", ID: "Idaho", IL: "Ill.", IN: "Ind.", KS: "Kan.",
    KY: "Ky.", LA: "La.", MA: "Mass.", MD: "Md.", ME: "Me.", MI: "Mich.",
    MN: "Minn.", MO: "Mo.", MS: "Miss.", MT: "Mont.", NC: "N.C.", ND: "N.D.",
    NE: "Neb.", NH: "N.H.", NJ: "N.J.", NM: "N.M.", NV: "Nev.", NY: "N.Y.",
    OH: "Ohio", OK: "Okla.", OR: "Ore.", PA: "Pa.", RI: "R.I.", SC: "S.C.",
    SD: "S.D.", TN: "Tenn.", TX: "Tex.", UT: "Utah", VA: "Va.", VT: "Vt.",
    WA: "Wash.", WI: "Wis.", WV: "W.Va.", WY: "Wyo."};

