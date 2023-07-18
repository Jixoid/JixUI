# JixUI
v4.9.6.1

Türkçe =====>

JixUI bir Lazarus paketidir.

SSL: https://indy.fulgan.com/SSL/
Nasıl yüklenir: Çok yakında...

Bir sorun veya öneri için Jixoid@gmail.com adresinden iletişime geçebilirsiniz.

Açıklamaya başlamadan önce teşekkür etmem gereken bir takım kişiler var:
- İlk önce bunu mümkün kılan BGRABitmapPack: https://github.com/bgrabitmap/BGRABitmapDelphi
- Sadece basit dokunuşlar için BGRAControls: https://github.com/bgrabitmap/bgracontrols
- Renk seçici için mbColorLib: https://github.com/nglthach/mbColorLib
- Ve son olarak Lazarus Forum: https://forum.lazarus.freepascal.org/index.php

Daha sonrasında söylemem gereken ise bu paketleri kendi paketimin bir parçası haline getirdim, ama bunu yapmamın tek nedeni bağımlılık sorunlarını çözmekti. Yani şuan bir bağımlılığı yok.
Bu paketlerde sadece biraz kırpma işlemi yaptım, bulmak isterseniz "Dependencies" klasöründe bulabilirsiniz.

Tamam, açıklamaya geçebiliriz

Asıl amacı hiç uğraşmadan temalar oluşturmak, uygulamak ve güzel bir arayüz hedeflemektedir.
Sizin yapmanız gereken şey tema şablonu oluşturmak, bu şablonu boyamak ve bileşenlere uygulamak JixUI'ın görevi.
Basit, Bileşenin ve Şablonun ThemeName'i uyuştuğunda temayı bileşene uygular.

Tasarım konusunda hiç şüphe etmeyin, özel editörler sayesinde zahmetsizce tasarım oluşturabilirsiniz.
"IgnoreTempStyle" ayarınıda unutmayın, bu ayar eğer True ise sadece renk uygulayacaktır, stilleri değiştirmez.

Şuan daha çok yeni, hatalar var biliyorum.
Güncellemeler gelicektir hataları bildirirseniz seviniriz.


English =====>

JixUI is a Lazarus package.

SSL: https://indy.fulgan.com/SSL/
How to install: Very soon...

For any issues or suggestions, you can contact us at Jixoid@gmail.com.

Before diving into the description, I would like to express my gratitude to the following individuals and projects:
- First and foremost, BGRABitmapPack, which made this possible: https://github.com/bgrabitmap/BGRABitmapDelphi
- BGRAControls, for providing simple touch interactions: https://github.com/bgrabitmap/bgracontrols
- mbColorLib, for the color picker: https://github.com/nglthach/mbColorLib
- And lastly, the Lazarus Forum: https://forum.lazarus.freepascal.org/index.php

What I should say next is that I made these packages part of my own package, but the only reason I did it was to solve dependency issues.  So there is no addiction at the moment.
 I just did some trimming on these packages, if you want to find them you can find them in the "Dependencies" folder.

Alright, let's move on to the description.

The main purpose of JixUI is to make theme creation, application, and achieving a beautiful interface effortless. Your task is to create a theme template, customize it, and apply it to the components. JixUI handles the process of applying the theme to the components when the Component's ThemeName matches the Template's ThemeName.

When it comes to design, rest assured that you can effortlessly create designs using the provided custom editors. Additionally, don't forget about the "IgnoreTempStyle" setting. If set to True, it will only apply the color and won't modify the styles.

Currently, JixUI is still in its early stages, and I acknowledge that there may be some bugs. We appreciate it if you report any issues you encounter, as updates will be forthcoming.
